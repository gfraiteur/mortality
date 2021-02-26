library(dplyr)
library(readr)
library(data.table)
library(ggridges)
library(ggplot2)
library(lubridate)
library(scales)
library(sqldf)
library(tidyr)
library(gt)
library(zoo)
library(lme4)

current_country_hmd_code = "BEL"
source("country.R")


### COVID DEATH

# Load COVID death

if ( !file.exists("./data/BEL/COVID19BE_MORT.csv")){
  download.file("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv", "./data/BEL/COVID19BE_MORT.csv")
}

covid_death <-
  read_csv("./data/BEL/COVID19BE_MORT.csv", col_types = cols(REGION = col_skip())) 

names(covid_death) <- c("day","age_covidgroup","sex", "covid_death")

# Group COVID data by week.
covid_death$date <- covid_death$day - wday(covid_death$day) - 1

covid_death <- covid_death %>%
  group_by(age_covidgroup, sex, date) %>%
  summarise(covid_death = sum(covid_death)) %>%
  ungroup()

# Complete missing values with 0.
covid_death <-covid_death %>%
  complete( sex, date, age_covidgroup, fill = list(covid_death = 0)  )


age_covidgroup =
  data.frame( age_covidgroup = c("0-24", "25-44", "45-64", "65-74", "75-84", "85+"),
              age_min = c(0, 25, 45, 65, 75, 85),
              age_max = c(24, 44, 64, 74, 84, age_end)  )


age_group = sqldf( "select age_group.*, age_covidgroup
             from age_group, age_covidgroup
             where age_group.age_min between age_covidgroup.age_min and age_covidgroup.age_max and
             age_group.age_max between age_covidgroup.age_min and age_covidgroup.age_max")

excess_death_vs_covid = 
  death_expected_weekly %>%
  merge( age_group, by = c("age_group") ) %>%
  merge( covid_death, by = c("sex", "date", "age_covidgroup") )

date_min = min(excess_death_vs_covid$date)
date_max = max(excess_death_vs_covid$date)+6

excess_death_vs_covid %>%
  group_by( age_covidgroup ) %>%
  summarise( covid_death = sum(covid_death) / sum(death_expected), 
             excess_death = sum(excess_death) / sum(death_expected)) %>%
  melt() %>%
  ggplot(aes(x=age_covidgroup , y=value, fill=variable)) +
  geom_col(position=position_dodge())  +
  scale_y_continuous(labels = scales::percent_format(accuracy = 01), name = "Mortality in % of expected mortality")  +
  labs( title = sprintf( "COVID-19-attributed mortality vs excess demographic mortality\nin Belgium between %s and %s", date_min, date_max) )

### TEMPERATURES

# https://opendata.meteo.be/geonetwork/srv/eng/catalog.search;jsessionid=B123D8FF9D843F6B8721B8878EB55479#/metadata/BEL/RMI_DATASET_AWS_1DAY
if ( !file.exists("./data/BEL/weather.csv")){
 download.file("https://opendata.meteo.be/service/aws/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=aws:aws_1day&outputFormat=csv", 
               "./data/BEL/weather.csv")
}

weather <- read_csv("./data/BEL/weather.csv", col_types = cols(FID = col_skip(), 
                                                         the_geom = col_skip(),
                                                         qc_flags = col_skip()))

weather$date <- as.Date( round_date(weather$timestamp) )
weather$code <- as.factor(weather$code)

### DEATH CAUSES

# https://statbel.fgov.be/fr/open-data/BEL/causes-de-deces-par-mois-sexe-groupe-dage-et-region

if ( !file.exists("./data/BEL/opendata_COD_cause.txt")){
  download.file("https://statbel.fgov.be/sites/default/files/files/opendata/BEL/COD/opendata_COD_cause.zip", "./data/BEL/opendata_COD_cause.zip")
  unzip("opendata_COD_cause.zip", exdir = "./data/BEL")
}

death_cause <- read_delim("data/BEL/opendata_COD_cause.txt", 
                          ";", escape_double = FALSE, col_types = cols(CD_RGN_REFNIS = col_skip()), 
                          trim_ws = TRUE)

names(death_cause) <- c("month_of_year", "year", "age_group", "sex", "cause", "death_observed")
death_cause$accidental <- (death_cause$cause == "V01-Y98")
death_cause <- death_cause %>% 
  group_by( year, month_of_year, age_group, sex, accidental ) %>%
  summarise( death_observed = sum(death_observed)) %>%
  ungroup()
  

death_cause$date <- make_date( death_cause$year,  death_cause$month_of_year, 1 )


accidental_death <-
  death_cause %>%
  group_by( year, age_group, sex, accidental ) %>%
  summarise( death_observed = sum(death_observed) ) %>%
  ungroup() %>%
  group_by( year, age_group, sex )  %>%
  mutate( percent_accidental_death = death_observed / sum(death_observed), all_death_observed = sum(death_observed) ) %>%
  filter( accidental == TRUE ) %>%
  select(-accidental) %>%
  rename( accidental_death_observed = death_observed )





# Classification of diseases: https://www.who.int/classifications/classification-of-diseases

# Variables: https://statbel.fgov.be/sites/default/files/files/opendata/BEL/COD/OpenDataVerklaring_V1.xlsx



### MODELLING

# Compute a liner regression for each age and sex
mortality_lr_coefficients <- transpose( data.frame(  mortality %>% 
                                                       filter( year >= 2009 ) %>%
                                                       group_by(sex, age) %>%
                                                       group_map( ~ c( .y$sex, .y$age, summary(lm( mortality ~ year, data = .x ))$coefficients[,1]) )))

names(mortality_lr_coefficients) <- c("sex", "age", "intercept", "year")
mortality_lr_coefficients$intercept <- as.double(mortality_lr_coefficients$intercept)
mortality_lr_coefficients$year <- as.double(mortality_lr_coefficients$year)

# Extend the population_structure dataset with death projections based on the mortality model
death_expected_yearly <- merge( population_structure, mortality_lr_coefficients, by = c("sex", "age")  )
names(death_expected_yearly) <- c( "sex", "age", "year", "population_count", "age_group", "intercept", "year_coefficient")
death_expected_yearly$death_expected_raw <- with(death_expected_yearly, population_count * ( intercept + (year * year_coefficient)))
death_expected_yearly <- death_expected_yearly[, c("sex", "age", "year", "death_expected_raw")]
death_expected_yearly <-
  merge( death_expected_yearly, age, by = "age") %>% 
  group_by(sex, age_group, year) %>%
  summarise( death_expected_raw = sum(death_expected_raw))

# Merge death projections with real death
death_expected_yearly <- merge(death_expected_yearly, death_yearly, by = c("sex", "age_group", "year") )

# Compute correction factors for the model
model_correction <- death_expected_yearly %>% 
  filter( year <= 2019 ) %>%
  group_by(age_group,sex) %>% 
  summarise( death_real = sum(death_observed_yearly), death_expected_raw = sum(death_expected_raw)) %>%
  ungroup()

model_correction$correction = model_correction$death_real / model_correction$death_expected_raw
model_correction <- model_correction[,c("age_group", "sex", "correction")]

# Apply correction factors
death_expected_yearly <- merge( death_expected_yearly, model_correction, by = c("age_group", "sex") )
death_expected_yearly$death_expected_yearly <- death_expected_yearly$death_expected_raw * death_expected_yearly$correction
death_expected_yearly <- death_expected_yearly %>% select(-death_expected_raw) %>% select(-correction) 


# Compute expected number of death per week
death_expected_weekly <- death %>% 
     merge( death_expected_yearly, by = c("year", "age_group", "sex")) %>%
     merge( death_per_week_of_year[,c("week_of_year", "age_group", "week_death_percent", "sex")], by = c("week_of_year", "age_group", "sex")) 
death_expected_weekly$death_expected = with(death_expected_weekly, death_expected_yearly * week_death_percent)
death_expected_weekly <- death_expected_weekly %>%
  select(-week_death_percent) %>%
  select(-death_expected_yearly)%>%
  select(-death_observed_yearly)  
death_expected_weekly$excess_death <- death_expected_weekly$death_observed - death_expected_weekly$death_expected





# Cumulative  excess mortality
cum_death_expected_weekly <- death_expected_weekly %>%
  arrange(age_group, sex, date)  %>%
  group_by( age_group, sex ) %>%
  mutate( "cum_excess_deaths" = cumsum(excess_death)) %>%
  arrange( age_group, sex, date )


# Graph: cumulative excess mortality

# Table: excess mortality in 2020
excess_death_2020 <-
  death_expected_weekly %>%
    filter(year==2020) %>% 
    group_by( age_group,sex ) %>%
    summarise( 
               expected_death = sum(death_expected), 
               excess_death = sum(excess_death), 
               death_observed = sum(death_observed),
               excess_death_percent = sum(excess_death) / sum(death_expected))

excess_death_2020 <-
  merge( excess_death_2020, population_structure_by_age_group %>% filter(year == 2020), by = c( "age_group", "sex" ) ) %>% 
  select(-year)

excess_death_2020$excess_mortality <- excess_death_2020$excess_death / excess_death_2020$population_count
excess_death_2020$expected_mortality <- excess_death_2020$expected_death  / excess_death_2020$population_count
excess_death_2020$observed_mortality <- excess_death_2020$death_observed / excess_death_2020$population_count
  


# Comparing excess deaths to COVID deaths
compared_covid_death <- merge( covid_death, death_expected_weekly, by = c("date", "age_group", "sex"), all.y = TRUE )




compared_covid_death %>%
  summarise( covid_death = sum(covid_death), excess_death = sum(excess_death) )
  


# Cause of mortality


mortality_cause <- merge( death_cause, population_structure_by_age_group, by = c("year", "age_group", "sex"))
mortality_cause$mortality <- mortality_cause$death_observed / mortality_cause$population_count

# Age group 0-24

cum_death_expected_weekly %>%
  filter( age_group == "0-24") %>%
  ggplot( aes( x = date, color = sex )) +
  geom_line(aes(y = cum_excess_deaths)) 

# Accidental deaths in age group 0-24 by pear
mortality_cause %>%
  filter( age_group == "0-24" ) %>%
  group_by( year, accidental, sex ) %>%
  summarise( mortality = sum( mortality)) %>%
  ggplot( aes( x = year, color = sex, linetype = accidental )) +
    geom_line(aes(y = mortality))  +
    scale_x_continuous(name = "year", breaks = 2009:2019) +
    scale_y_continuous(labels = scales::percent_format(), name = "mortality", limits = c(0,NA)) 


# Extrapolate accidental death to 2020
accidental_death_lr_coefficients <- transpose( data.frame(  accidental_death %>% 
                                                       group_by(sex, age_group) %>%
                                                       group_map( ~ c( .y$sex, .y$age_group, coefficients(lm( percent_accidental_death ~ year, data = .x ))) )))
  
names(accidental_death_lr_coefficients) <- c("sex", "age_group", "intercept", "year_coefficient")
accidental_death_lr_coefficients$intercept <- as.double(accidental_death_lr_coefficients$intercept)
accidental_death_lr_coefficients$year_coefficient <- as.double(accidental_death_lr_coefficients$year_coefficient)


accidental_death_2020 <-
  accidental_death_lr_coefficients %>%
    filter( age_group == "0-24" ) %>%
    mutate( percent_accidental_death = intercept + 2020 * year_coefficient )



### MORTALITY VARIANCE

mortality_deviation <-
  mortality %>%
    group_by( year, sex, age_group ) %>%
    summarise( mortality = mean(mortality)) %>%
    group_by( sex, age_group ) %>%
    mutate( mortality_rollmean = rollmedian( mortality, 5, align="center", na.pad=TRUE ))

mortality_deviation$deviation <-
  mortality_deviation$mortality - mortality_deviation$mortality_rollmean
mortality_deviation$relative_deviation <-
  mortality_deviation$mortality / mortality_deviation$mortality_rollmean - 1



  