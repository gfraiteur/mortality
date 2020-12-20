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

rm(list=ls())

if ( !file.exists("Belgium.R")){
  stop("Change the working directory to the repo directory.")
}
  

# Generate time dimension (years, weeks)
weeks <- merge( 2009:2020,  1:52, all=TRUE )
names(weeks) <- c("year", "week_of_year")
weeks$week <- sprintf( "%i-W%02d", weeks$year, weeks$week_of_year )
weeks$date <- make_date( weeks$year ) + (weeks$week_of_year - 1) * 7

# Generate age groups
age_group <- data.frame( age_group = c("0-24", "25-44", "45-64", "65-74", "75-84", "85+"),
                         age_min =   c(0,      25,      45,      65,       75,        85),
                         age_max =   c(24,    44,       64,      74,       84,       110) )
age = data.frame( age = 0:110 )
age <- sqldf( "select age, age_group from age, age_group where age.age between age_group.age_min and age_group.age_max")

### POPULATION STRUCTURE

# Load population_structure structure
if ( exists("population_structure"))  {
  rm(population_structure)
}
for ( year in 2009:2020 ) {
  message("Loading year ", year)
  filename <- sprintf("./data/TF_SOC_POP_STRUCT_%d.txt", year)
  if ( !file.exists(filename)) {
    zipfilename <- sprintf("./data/TF_SOC_POP_STRUCT_%d.zip", year)
    if ( !file.exists(zipfilename)) {
      url <- sprintf("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking naar woonplaats, nationaliteit burgelijke staat , leeftijd en geslacht/TF_SOC_POP_STRUCT_%d.zip", year)
      download.file(url, zipfilename)
    }
    setwd("data")
    unzip(zipfilename)
    setwd("..")
  }
  
  separator <- if ( year == 2020 ) { ";" } else { "|" }
  population_structure_new <- read_delim(filename, separator, escape_double = FALSE, trim_ws = TRUE)
  population_structure_new <- population_structure_new[,c("CD_SEX","CD_AGE","MS_POPULATION")]
  names(population_structure_new) <- c("sex", "age", "population_count")
  population_structure_new$year = year
  
  if ( exists("population_structure"))  {
    population_structure <- rbind(population_structure, population_structure_new )
  }
  else {
    population_structure <- population_structure_new
  }
  rm(population_structure_new)
}

# Removing irrelevant details from the population_structure dataset
population_structure <- population_structure %>% 
  group_by(sex, age, year) %>% 
  summarise( population_count = sum(population_count)) %>%
  ungroup()

# Joining with age groun
population_structure <- merge( population_structure, age, by = c("age") )

population_structure_by_age_group <-
  population_structure %>%
  group_by( year, sex, age_group)  %>%
  summarise( population_count = sum(population_count))
  

### DEATH

# Load death data
death_2020_max_week <- 49
death_filename  <- sprintf("./data/DEMO_DEATH_OPEN_W%d.txt", death_2020_max_week)
if ( !file.exists(death_filename)){
  download.file("https://statbel.fgov.be/sites/default/files/files/opendata/deathday/DEMO_DEATH_OPEN.zip", "./data/DEMO_DEATH_OPEN.zip")
  setwd("data")
  unzip("DEMO_DEATH_OPEN.zip")
  setwd("..")
}

death <- read_delim(death_filename, ";", escape_double = FALSE, trim_ws = TRUE)
death <- death[,c("CD_SEX","CD_AGEGROUP","NR_WEEK","MS_NUM_DEATH")]
names(death) <- c("sex", "age_group", "week", "death_observed")
death$sex[death$sex==1] <- "M"
death$sex[death$sex==2] <- "F"
death <- death %>% complete( sex, age_group, week, fill = list(death_observed = 0)  )
death <- death %>%
  group_by( sex, age_group, week ) %>% 
  summarise( death_observed = sum(death_observed)) %>%
  ungroup()

# Add week info to DEATH
death <- merge( death, weeks, by.x = "week", by.y = "week")
death_yearly <- death %>% group_by( sex, age_group, year ) %>% summarise( death_observed_yearly = sum(death_observed))

# Compute the histogram of deaths per week excluding year 2020. 
# This model is over-fitted if we try to match every week, so we won't group by sex.
death_per_week_of_year <- death %>% 
  group_by( age_group, sex ) %>% 
  arrange( age_group, sex, date ) %>%
  mutate( death_observed_rollmean = rollmean(death_observed, 3, fill = NA)) %>%
  filter(  year <= 2019)  %>%
  group_by( age_group, sex, week_of_year ) %>%
  summarise( 
    avg_death_in_week_of_year = mean(death_observed_rollmean, na.rm = TRUE) ) %>%
  ungroup() %>%
  group_by( age_group, sex ) %>%
  mutate( week_death_percent = avg_death_in_week_of_year / sum(avg_death_in_week_of_year)  )



### MORTALITY

# Load mortality data

# You need a password to download this file.
# download.file("https://www.mortality.org/hmd/BEL/STATS/Mx_1x1.txt", "hmd_BEL_STATS_Mx_1x1.txt" )

mortality <- read_table2("./data/hmd_BEL_STATS_Mx_1x1.txt", skip = 1)

mortality_male <- mortality[, c("Year", "Age", "Male")]
names(mortality_male) <- c("year", "age", "mortality")
mortality_male$sex <- 'M'

mortality_female <- mortality[, c("Year", "Age", "Female")]
names(mortality_female) <- c("year", "age", "mortality")
mortality_female$sex <- 'F'

mortality <- rbind(mortality_female, mortality_male)
rm(mortality_female)
rm(mortality_male)

mortality$mortality <- as.double(mortality$mortality)
mortality$age <- as.double(mortality$age)  # This will convert 110+ to N/A

mortality <- na.omit(mortality) # This removes N/A

mortality <- merge( mortality, age, by = c("age"))

mortality_all <- mortality

mortality <- filter( mortality, year >= 2009 )




### COVID DEATH

# Load COVID death

if ( !file.exists("./data/COVID19BE_MORT.csv")){
  download.file("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv", "./data/COVID19BE_MORT.csv")
}

covid_death <- read_csv("./data/COVID19BE_MORT.csv", col_types = cols(REGION = col_skip()))
names(covid_death) <- c("day","age_group","sex", "covid_death")

# Group COVID data by week.
covid_death$date <- make_date( 2020 ) + (week(covid_death$day) - 1) * 7 # Key the data by week instead of 1 day

covid_death <- covid_death %>%
  group_by(age_group, sex, date) %>%
  summarise(covid_death = sum(covid_death)) %>%
  ungroup()

# Complete missing values with 0.
covid_death <-covid_death %>%
  complete( sex, date, age_group, fill = list(covid_death = 0)  )


### TEMPERATURES

# https://opendata.meteo.be/geonetwork/srv/eng/catalog.search;jsessionid=B123D8FF9D843F6B8721B8878EB55479#/metadata/RMI_DATASET_AWS_1DAY
if ( !file.exists("./data/weather.csv")){
 download.file("https://opendata.meteo.be/service/aws/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=aws:aws_1day&outputFormat=csv", 
               "./data/weather.csv")
}

weather <- read_csv("./data/weather.csv", col_types = cols(FID = col_skip(), 
                                                         the_geom = col_skip(),
                                                         qc_flags = col_skip()))

weather$date <- as.Date( round_date(weather$timestamp) )
weather$code <- as.factor(weather$code)

### DEATH CAUSES

# https://statbel.fgov.be/fr/open-data/causes-de-deces-par-mois-sexe-groupe-dage-et-region

if ( !file.exists("./data/opendata_COD_cause.txt")){
  download.file("https://statbel.fgov.be/sites/default/files/files/opendata/COD/opendata_COD_cause.zip", "./data/opendata_COD_cause.zip")
  setwd("data")
  unzip("opendata_COD_cause.zip")
  setwd("..")
}

death_cause <- read_delim("data/opendata_COD_cause.txt", 
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

# Variables: https://statbel.fgov.be/sites/default/files/files/opendata/COD/OpenDataVerklaring_V1.xlsx



### MODELLING

# Compute a liner regression for each age and sex
mortality_lr_coefficients <- transpose( data.frame(  mortality %>% 
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
compared_covid_death <- merge( covid_death, death_expected_weekly, by = c("date", "age_group", "sex") )




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
accidental_death_lr_coefficients$intercept = as.double(accidental_death_lr_coefficients$intercept)
accidental_death_lr_coefficients$year_coefficient = as.double(accidental_death_lr_coefficients$year_coefficient)


accidental_death_2020 <-
  accidental_death_lr_coefficients %>%
    filter( age_group == "0-24" ) %>%
    mutate( percent_accidental_death = intercept + 2020 * year_coefficient )



### MORTALITY VARIANCE

mortality_deviation <-
  mortality_all %>%
    group_by( year, sex, age_group ) %>%
    summarise( mortality = mean(mortality)) %>%
    group_by( sex, age_group ) %>%
    mutate( mortality_rollmean = rollmedian( mortality, 5, align="center", na.pad=TRUE ))

mortality_deviation$deviation <-
  mortality_deviation$mortality - mortality_deviation$mortality_rollmean
mortality_deviation$relative_deviation <-
  mortality_deviation$mortality / mortality_deviation$mortality_rollmean - 1



  