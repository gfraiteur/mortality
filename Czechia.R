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
library(readxl)
library(demography)

# Remove everything in the environment but not the data we downloaded from mortality.org.
rm(
  list= setdiff(ls(), c("hmd_mx", "hmd_pop"))
  )

source("passwords.R", local = TRUE )


if ( !file.exists("Czechia.R")){
  stop("Change the working directory to the repo directory.")
}

if (!dir.exists("./data/Czechia"))  {
  dir.create("./data/Czechia")
}


# Generate time dimension (years, weeks)
weeks <- merge( 2005:2020,  1:52, all=TRUE )
names(weeks) <- c("year", "week_of_year")
weeks$week <- sprintf( "%i-W%02d", weeks$year, weeks$week_of_year )
weeks$date <- make_date( weeks$year ) + (weeks$week_of_year - 1) * 7

# Generate major age groups
age_group <- data.frame( age_group = c("0-24", "25-44", "45-64", "65-74", "75-84", "85+"),
                         age_min =   c(0,      25,      45,      65,       75,        85),
                         age_max =   c(24,    44,       64,      74,       84,       110) )
age = data.frame( age = 0:110 )
age <- sqldf( "select age, age_group from age, age_group where age.age between age_group.age_min and age_group.age_max")

# Generate 5-year age groups (used for population structure). These groups are used by the Czech institute for statistics to report deaths. Other data is by age.
minor_age_group <- data.frame( age_min = seq(0,85,5) )
minor_age_group$age_max <- minor_age_group$age_min + 4
minor_age_group$minor_age_group <- sprintf("%d-%d", minor_age_group$age_min, minor_age_group$age_max)
minor_age_group$minor_age_group_sortable <- sprintf("%02d-%02d", minor_age_group$age_min, minor_age_group$age_max)
minor_age_group <- rbind( minor_age_group, 
                          data.frame(
                            minor_age_group = c( "90+"),
                            minor_age_group_sortable = c( "90+"),
                            age_min = c( 90),
                            age_max = c(110)
                          )) %>%
  arrange( age_min)

age <- sqldf( "select age, age_group, minor_age_group, minor_age_group_sortable from age, minor_age_group where age.age between minor_age_group.age_min and minor_age_group.age_max")
minor_age_group <- sqldf( "select minor_age_group, minor_age_group_sortable, age_group, minor_age_group.age_min, minor_age_group.age_max from age_group, minor_age_group where minor_age_group.age_min between age_group.age_min and age_group.age_max and minor_age_group.age_max between age_group.age_min and age_group.age_max")


### POPULATION STRUCTURE

if ( !exists("hmd_pop")) {
  hmd_pop <- hmd.pop("CZE", hmd_username, hmd_password)
}


population_structure_age_male <- hmd_pop$pop$male
population_structure_age_male <- melt(population_structure_age_male, id = c("age"))
names(population_structure_age_male) <- c("age", "year", "population_count")
population_structure_age_male$sex <- "M"

population_structure_age_female <- hmd_pop$pop$female
population_structure_age_female <- melt(population_structure_age_female, id = c("age"))
names(population_structure_age_female) <- c("age", "year", "population_count")
population_structure_age_female$sex <- "F"

population_structure <- rbind(population_structure_age_female, population_structure_age_male )

# By converting the age to an int, we remove the 110+ age group as a side effect.
population_structure$age <- as.integer(as.character(population_structure$age))
population_structure <- population_structure[complete.cases(population_structure),]

rm( population_structure_age_female, population_structure_age_male )


# Extrapolate the data for January 1st, 2020

population_structure_2020_lr <- transpose( data.frame(  population_structure %>% 
                                                          filter( year >= 2014 ) %>%
                                                              group_by(sex, age) %>%
                                                              group_map( ~ c(  .y$age, .y$sex, coefficients(lm( population_count ~ year, data = .x ))) )))

names(population_structure_2020_lr) <- c("age", "sex", "intercept", "year_coefficient")

population_structure_2020_lr$intercept <- as.double(population_structure_2020_lr$intercept)
population_structure_2020_lr$year_coefficient <- as.double(population_structure_2020_lr$year_coefficient)
population_structure_2020_lr$population_count <- population_structure_2020_lr$intercept + 2020 * population_structure_2020_lr$year_coefficient
population_structure_2020_lr$year <- 2020

population_structure <- rbind( population_structure,
                               population_structure_2020_lr %>% 
                                 select(age, year, population_count, sex ))


# Joining with age group
population_structure <- merge( population_structure, age, by = c("age") )


population_structure_by_age_group <-
  population_structure %>%
  group_by( year, sex, age_group)  %>%
  summarise( population_count = sum(population_count))
  

### DEATH

# Load death data


if ( !file.exists("./data/Czechia/demomigr_2005_2019_komplet.csv")) {
  download.file("https://www.czso.cz/documents/10180/138290020/demomigr_2005_2019_komplet.zip/eb3d3766-b242-4bae-bffb-68a0e13a49a1?version=1.1", "./data/Czechia/demomigr_2005_2019_komplet.zip", mode = "wb")
  unzip("./data/Czechia/demomigr_2005_2019_komplet.zip", exdir = "./data/Czechia")
}

death_1 <-
  read_csv("./data/Czechia/DEMOMIGR_2005_2019_komplet.csv", 
           col_types = cols(uzemi = col_skip(), 
                            status = col_skip(), 
                            uzemi_text = col_skip(), 
                            pohlavi_text = col_skip(),
                            vek_text = col_skip()))


death_2020_max_week <- 47
death_filename_zip  <- sprintf("./data/Czechia/demomigr_2020_predbezna_20t%d.zip", death_2020_max_week)
death_filename_csv  <- sprintf("./data/Czechia/demomigr_2020_predbezna_20t%d.csv", death_2020_max_week)
death_url  <- sprintf("https://www.czso.cz/documents/10180/138290026/demomigr_2020_predbezna_20t%d.zip/091774d2-179e-4c7c-b6d4-e703884de732?version=1.1", death_2020_max_week)

if ( !file.exists(death_filename_csv)) {
  download.file(death_url, death_filename_zip, mode = "wb")
  unzip(death_filename_zip, exdir = "./data/Czechia")
}




death_2 <-
  read_csv(death_filename_csv, 
           col_types = cols(uzemi = col_skip(), 
                            status = col_skip(), 
                            uzemi_text = col_skip(), 
                            pohlavi_text = col_skip(),
                            status_text = col_skip(),
                            vek_text = col_skip()))



death <- rbind( death_1, death_2)

rm ( death_1, death_2 )

names(death) <- c("week", "minor_age_group", "sex", "death_observed")

death$minor_age_group <- str_replace( death$minor_age_group, "Y_GE90", "90+" )
death$minor_age_group <- str_replace( str_replace(death$minor_age_group, "Y", "" ), "T", "-")


death <- death %>%
  complete( sex, minor_age_group, week, fill = list(death_observed = 0)  )

death <- merge( death, minor_age_group, by = "minor_age_group") %>% 
  group_by( sex, minor_age_group, week ) %>% 
  summarise( death_observed = sum(death_observed)) %>%
  ungroup()

# Add week info to DEATH
death <- merge( death, weeks, by.x = "week", by.y = "week") %>%
  filter( year >= 2009 ) # Keep 10 years history


death_yearly <- death %>% 
  group_by( sex, minor_age_group, year ) %>% 
  summarise( death_observed_yearly = sum(death_observed))

# Compute the histogram of deaths per week excluding year 2020. 
death_per_week_of_year <- death %>% 
  group_by( minor_age_group, sex ) %>% 
  arrange( minor_age_group, sex, date ) %>%
  mutate( death_observed_rollmean = rollmean(death_observed, 3, fill = NA)) %>%
  filter(  year <= 2019)  %>%
  group_by( minor_age_group, sex, week_of_year ) %>%
  summarise( 
    avg_death_in_week_of_year = mean(death_observed_rollmean, na.rm = TRUE) ) %>%
  ungroup() %>%
  group_by( minor_age_group, sex ) %>%
  mutate( week_death_percent = avg_death_in_week_of_year / sum(avg_death_in_week_of_year)  )



### MORTALITY

# Load mortality data


if ( !exists("hmd_mx")) {
  hmd_mx <- hmd.mx("CZE", hmd_username, hmd_password)
  # hmd_mx$pop also contains population data, but not for year 2019.
}


mortality_male <- melt(hmd_mx$rate$male, id = c("age"))
names(mortality_male) <- c("age", "year", "mortality")
mortality_male$sex <- 'M'

mortality_female <- melt(hmd_mx$rate$female, id = c("age"))
names(mortality_female) <- c("age", "year", "mortality")
mortality_female$sex <- 'F'

mortality <- rbind(mortality_female, mortality_male)
rm(mortality_female)
rm(mortality_male)

mortality <- 
  mortality %>%
  arrange( age, sex, year ) %>%
  group_by( age, sex ) %>%
  mutate( mortality = na.approx( mortality, na.rm = FALSE ))

mortality$age <- as.integer(as.character(mortality$age))  

mortality <- mortality[complete.cases(mortality),]

mortality <- merge( mortality, age, by = c("age")) 



# 
# 
# ### COVID DEATH
# 
# # Load COVID death
# 
if ( !file.exists("./data/Czechia/umrti.csv")){
 download.file("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/umrti.csv", "./data/Czechia/umrti.csv")
}

covid_death <- read_csv("data/Czechia/umrti.csv", 
                                 col_types = cols(kraj_nuts_kod = col_skip(), 
                                                  okres_lau_kod = col_skip()))

names(covid_death) <- c("day","age","sex")
covid_death$covid_death <- 1
covid_death$sex <- str_replace(covid_death$sex, "Z", "F" )

# Group COVID data by week.
covid_death$date <- make_date( 2020 ) + (week(covid_death$day) - 1) * 7 # Key the data by week instead of 1 day

covid_death <- covid_death %>%
 group_by(age, sex, date) %>%
 summarise(covid_death = sum(covid_death)) %>%
 ungroup()


### MODELLING

# Compute a liner regression for each age and sex
mortality_lr_coefficients <- transpose( data.frame(  mortality %>% 
                                                       filter( year >= 2009 ) %>%
                                                       group_by(sex, age) %>%
                                                       group_map( ~ c( .y$sex, .y$age, summary(lm( mortality ~ year, data = .x ))$coefficients[,1]) )))

names(mortality_lr_coefficients) <- c("sex", "age", "intercept_coeff", "year_coeff")
mortality_lr_coefficients$intercept_coeff <- as.double(mortality_lr_coefficients$intercept_coeff)
mortality_lr_coefficients$year_coeff <- as.double(mortality_lr_coefficients$year_coeff)

# Extend the population_structure dataset with death projections based on the mortality model
death_expected_yearly <- merge( population_structure, mortality_lr_coefficients, by = c("sex", "age")  ) 
death_expected_yearly$death_expected_raw <- 
  with(death_expected_yearly, population_count * ( intercept_coeff + (year * year_coeff)))

death_expected_yearly <- death_expected_yearly %>%
    select( -intercept_coeff, -year_coeff )

death_expected_yearly <- death_expected_yearly %>%
  group_by(sex, minor_age_group, year) %>%
  summarise( death_expected_raw = sum(death_expected_raw))

# Merge death projections with real death
death_expected_yearly <- merge(death_expected_yearly, death_yearly, by = c("sex", "minor_age_group", "year") )

# Compute correction factors for the model
model_correction <- death_expected_yearly %>% 
  filter( year <= 2019 ) %>%
  group_by(minor_age_group,sex) %>% 
  summarise( death_real = sum(death_observed_yearly), death_expected_raw = sum(death_expected_raw)) %>%
  ungroup()

model_correction$correction = model_correction$death_real / model_correction$death_expected_raw
model_correction <- model_correction[,c("minor_age_group", "sex", "correction")]

# Apply correction factors
death_expected_yearly <- merge( death_expected_yearly, model_correction, by = c("minor_age_group", "sex") )
death_expected_yearly$death_expected_yearly <- death_expected_yearly$death_expected_raw * death_expected_yearly$correction
death_expected_yearly <- death_expected_yearly %>% select(-death_expected_raw) %>% select(-correction) 


# Compute expected number of death per week
death_expected_weekly <- death %>% 
     merge( death_expected_yearly, by = c("year", "minor_age_group", "sex")) %>%
     merge( death_per_week_of_year[,c("week_of_year", "minor_age_group", "week_death_percent", "sex")], by = c("week_of_year", "minor_age_group", "sex")) 
death_expected_weekly$death_expected = with(death_expected_weekly, death_expected_yearly * week_death_percent)
death_expected_weekly <- death_expected_weekly %>%
  select(-week_death_percent) %>%
  select(-death_expected_yearly)%>%
  select(-death_observed_yearly)  
death_expected_weekly$excess_death <- death_expected_weekly$death_observed - death_expected_weekly$death_expected





# Cumulative  excess mortality
cum_death_expected_weekly <- death_expected_weekly %>%
  arrange(minor_age_group, sex, date)  %>%
  group_by( minor_age_group, sex ) %>%
  mutate( "cum_excess_deaths" = cumsum(excess_death)) %>%
  arrange( minor_age_group, sex, date )


# Table: excess mortality in 2020
excess_death_2020 <-
  death_expected_weekly %>%
    filter(year==2020) %>% 
    group_by( minor_age_group,sex ) %>%
    summarise( 
               expected_death = sum(death_expected), 
               excess_death = sum(excess_death), 
               death_observed = sum(death_observed),
               excess_death_percent = sum(excess_death) / sum(death_expected))

excess_death_2020 <-
  merge( excess_death_2020, 
         population_structure %>%
           filter(year == 2020) %>%
           group_by( year, sex, minor_age_group)  %>%
           summarise( population_count = sum(population_count)),
         by = c( "minor_age_group", "sex" ) ) %>% 
  select(-year)

excess_death_2020$excess_mortality <- excess_death_2020$excess_death / excess_death_2020$population_count
excess_death_2020$expected_mortality <- excess_death_2020$expected_death  / excess_death_2020$population_count
excess_death_2020$observed_mortality <- excess_death_2020$death_observed / excess_death_2020$population_count
  


# Comparing excess deaths to COVID deaths
compared_covid_death <- covid_death %>%
  merge( age) %>%
  group_by( date, sex, minor_age_group ) %>%
  summarise( covid_death = sum(covid_death)) %>%
  merge( death_expected_weekly %>% 
           filter( year == 2020 ),
         by = c("date", "minor_age_group", "sex"), 
         all.y = TRUE )
compared_covid_death[is.na(compared_covid_death)] <- 0


cum_compared_covid_death <- compared_covid_death %>%
  complete( minor_age_group, sex, date, fill =list(excess_death = 0, covid_death = 0 ) ) %>%
  arrange(minor_age_group, sex, date)  %>%
  group_by( minor_age_group, sex ) %>%
  mutate( "cum_excess_death" = cumsum(excess_death), "cum_covid_death" = cumsum(covid_death)) %>%
  arrange( minor_age_group, sex, date )




  #summarise( covid_death = sum(covid_death), excess_death = sum(excess_death) )
  


# Cause of mortality


# mortality_cause <- merge( death_cause, population_structure_by_age_group, by = c("year", "age_group", "sex"))
# mortality_cause$mortality <- mortality_cause$death_observed / mortality_cause$population_count

# Age group 0-24

#cum_death_expected_weekly %>%
  #filter( age_group == "0-24") %>%
  #ggplot( aes( x = date, color = sex )) +
  #geom_line(aes(y = cum_excess_deaths)) 

# # Accidental deaths in age group 0-24 by pear
# mortality_cause %>%
#   filter( age_group == "0-24" ) %>%
#   group_by( year, accidental, sex ) %>%
#   summarise( mortality = sum( mortality)) %>%
#   ggplot( aes( x = year, color = sex, linetype = accidental )) +
#     geom_line(aes(y = mortality))  +
#     scale_x_continuous(name = "year", breaks = 2009:2019) +
#     scale_y_continuous(labels = scales::percent_format(), name = "mortality", limits = c(0,NA)) 
# 
# 
# # Extrapolate accidental death to 2020
# accidental_death_lr_coefficients <- transpose( data.frame(  accidental_death %>% 
#                                                        group_by(sex, age_group) %>%
#                                                        group_map( ~ c( .y$sex, .y$age_group, coefficients(lm( percent_accidental_death ~ year, data = .x ))) )))
#   
# names(accidental_death_lr_coefficients) <- c("sex", "age_group", "intercept", "year_coefficient")
# accidental_death_lr_coefficients$intercept = as.double(accidental_death_lr_coefficients$intercept)
# accidental_death_lr_coefficients$year_coefficient = as.double(accidental_death_lr_coefficients$year_coefficient)
# 
# 
# accidental_death_2020 <-
#   accidental_death_lr_coefficients %>%
#     filter( age_group == "0-24" ) %>%
#     mutate( percent_accidental_death = intercept + 2020 * year_coefficient )
# 
# 

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



  
  

### PENSION



if ( !file.exists("./data/Czechia/duchody-dle-veku.csv")) {
  download.file("https://data.cssz.cz/dump/duchody-dle-veku.csv", "./data/Czechia/duchody-dle-veku.csv" )
}

pension <- read_csv("data/Czechia/duchody-dle-veku.csv", col_types = cols(
  druh_duchodu_kod = col_character(),
  druh_duchodu = col_character(),
  pohlavi_kod = col_skip(),
  pohlavi = col_character(),
  referencni_obdobi = col_date(format = ""),
  vek_kod = col_character(),
  vek = col_skip(),
  pocet_duchodu = col_double()
) )


names(pension) <- c("kind", "kind_text", "sex", "date", "minor_age_group", "pension_count")

pension_age_groups <- pension %>% group_by(minor_age_group) %>% summarise()

pension_kinds <-
  pension %>% group_by(kind, kind_text) %>% summarise()

# Documentation of pension kinds: https://data.cssz.cz/documentation/pomocne-ciselniky

pension$year <- as.integer( year(pension$date) )
pension$sex <- ifelse( pension$sex=="Žena", "F", ifelse( pension$sex=="Muž", "M", NA))
pension <- pension %>%
  select(-date) %>%
  filter( kind %in% c("PK_S", "PK_SI", "PK_SRN", "PK_ST", "PK_SD", "PK_SR", "PK_SV", "PK_SIV", "PK_SRNV", "PK_STV", "PK_SDV", "PK_SRV", "PK_SVM", "PK_SIVM", "PK_SRNVM", "PK_STVM", "PK_SDVM", "PK_SRVM")) %>%
  group_by( minor_age_group, sex, year ) %>%
  summarise(pension_count = sum(pension_count)) 
 
# Take only the age groups from minor_age_group. This has the side effect of removing the age group 0-19.
pension <- pension %>%
  merge(minor_age_group %>% select( minor_age_group))
  

pension <-
 pension %>%
   merge( population_structure %>%
            group_by(minor_age_group, sex, year) %>%
            summarise(population_count = sum(population_count))
            )
 
pension$pension_percent <- pension$pension_count / pension$population_count

pension %>%
  filter(year==2019) %>%
  group_by(  minor_age_group, sex ) %>%
  summarise( pension_percent = sum(pension_count) / sum(population_count)) %>%
  ggplot( aes(y = pension_percent, x=minor_age_group, fill = sex) )  +
  geom_bar(stat='identity', position='dodge') 
  

# Where do seniors live? https://www.czso.cz/csu/czso/6b004993af
