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

if ( !exists("country")) {
  stop("The 'country' variable is not defined.")
}


# Remove everything in the environment but not the data we downloaded from mortality.org.
rm(
  list= setdiff(ls(), c("hmd_mx", "hmd_pop", "country", "all_graphs"))
  )


# Read countries
countries <- read_csv("HMD-countries-codes.csv")

country_name <- (countries %>% filter( code == country ))$name
country_iso_code <- coalesce( (countries %>% filter( code == country ))$iso_code, country )

# Functions to work with graphs
graphs <- list()

if ( !exists("all_graphs")) {
 all_graphs <- list() 
}

this_theme <- function() {
  ggtitle(caption =  country_name) + theme_minimal() + theme(legend.position='bottom')
}

append_graph <- function(name) {
  graphs[[name]] <<- last_plot()
}

source("passwords.R", local = TRUE )


directory <- paste("./data/", country, sep = "")

if (!dir.exists(directory))  {
  dir.create(directory)
}



# Generate time dimension (years, weeks)
weeks <- merge( 2005:2020,  1:52, all=TRUE )
names(weeks) <- c("year", "week_of_year")
weeks$week <- sprintf( "%i-W%02d", weeks$year, weeks$week_of_year )
weeks$date <- make_date( weeks$year ) + (weeks$week_of_year - 1) * 7

### LOAD HMD DATA

has_hmd <- exists("hmd_pop") & exists("hmd_mx")
if ( has_hmd ) {
  has_hmd <- hmd_pop$label == country
}

if ( !has_hmd  ) {
  hmd_pop <- hmd.pop(country, hmd_username, hmd_password)
  hmd_mx <- hmd.mx(country, hmd_username, hmd_password)
}
 

### DEATH

# Load death das.integer(death$AgeInterval)ata

death_filename_zip = "./data/STMFinput.zip"
if ( !file.exists(death_filename_zip)){
  download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", death_filename_zip )
  unzip(death_filename_zip, exdir = "./data")
}

death <-
  read_csv(sprintf("data/%sstmf.csv", country), col_types = cols(
      Year = col_integer(), 
      Week = col_integer() ))

# If we have sex info, remove the data for 'both' gender.
has_sex_category <- 'm' %in% death$Sex
if ( has_sex_category ) {
  sex <- c("M", "F")
  death <- death %>% filter( Sex != 'b')
} else {
  sex <- c("B")
}
  


death <- death %>%
  filter( !is.na(as.integer(death$Age)) & Year >= 2009) %>%
  select( -Type, -Access )

# Generate age groups from death file
death$age_min <- as.integer(death$Age)
death$age_max <-  ifelse( death$AgeInterval == "+", 120, as.integer(death$Age) + as.integer(death$AgeInterval) - 1 )
death$age_group <- ifelse( death$AgeInterval == "+" , paste( death$Age, "+", sep = ""), sprintf("%02d-%02d", as.integer( death$Age ), as.integer( death$Age ) + as.integer(death$AgeInterval) - 1)  )

age = data.frame( age = 0:120 )

age_group <- death %>%
  group_by( age_min, age_max, age_group ) %>%
  summarise( year_count = n_distinct(Year) ) %>% 
  ungroup() %>% 
  arrange(age_min)

# In Austria, we have two age groups used for temporary data: 0-64 and 65+. We remove these groups.
if ( country == "AUT") {
  death <- death %>% filter( !(age_group %in% c("00-64", "65+") ) )
  age_group <- age_group %>% filter( !(age_group %in% c("00-64", "65+") ) )
}


# French data have overlapping age groups. Need to select supersets.
overlapping_age_group <- sqldf( "select ag1.age_group ag1, ag1.age_min min1, ag1.age_max max1, ag2.age_group ag2, ag2.age_min min2, ag2.age_max max2 from age_group ag1, age_group ag2  where (ag1.age_min between ag2.age_min and ag2.age_max) and ag1.age_group <> ag2.age_group")
overlapping_age_group$subset <- overlapping_age_group$min2 >= overlapping_age_group$min1 & overlapping_age_group$max2 <= overlapping_age_group$max1
overlapping_age_group$superset <- overlapping_age_group$min1 >= overlapping_age_group$min2 & overlapping_age_group$max1 <= overlapping_age_group$max2
overlapping_age_group <- overlapping_age_group %>% filter( subset == FALSE )

if ( nrow(overlapping_age_group %>% filter( superset == FALSE)) > 0 ) {
  stop("There are overlapping age groups.")
}

age_group <- age_group %>% filter( !(age_group %in% overlapping_age_group$ag1) )

if ( nrow(overlapping_age_group) > 0 ) {
  for(i in 1:nrow(overlapping_age_group)) {
    print(paste("Replacing age group", overlapping_age_group[i,]$ag1))
    death <-
      death %>%
        mutate( age_group = if_else( age_group ==  overlapping_age_group[i,]$ag1, overlapping_age_group[i,]$ag2, age_group ) )
  }
}



age <- sqldf( "select age,  age_group from age, age_group where age.age between age_group.age_min and age_group.age_max")

max_week_2020 <- max( (death %>% filter(Year == 2020))$Week )


death$week <- sprintf( "%i-W%02d", death$Year, death$Week )  

death <- death %>%
  select( -Age, -AgeInterval, -Area, -Year, -Week, -age_min, -age_max ) %>%
  rename( sex = Sex, death_observed = Deaths )

death$sex <- toupper(death$sex)

death <- death %>%
  group_by( sex, age_group, week ) %>% 
  summarise( death_observed = sum(death_observed)) %>%
  ungroup() %>%
  complete( sex, age_group, week, fill = list(death_observed = 0)  ) 

# Add week info to DEATH
death <- merge( death, weeks, by.x = "week", by.y = "week")


death_yearly <- death %>% 
  group_by( sex, year, age_group ) %>% 
  summarise( death_observed_yearly = sum(death_observed))


# Graph yearly deaths

death %>%
  filter ( year < 2020 ) %>%
  group_by( year ) %>%
  summarize( death_observed = sum( death_observed )) %>%
  ggplot( aes( x = year, y = death_observed)) + 
  geom_line(aes()) + 
  ggtitle("Number of deaths per year") +
  scale_x_continuous( breaks = 2005:2019,name = "year") +
  scale_y_continuous( labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths") +
  this_theme()

append_graph("death")

# Compute the histogram of deaths per week excluding year 2020. 
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




### POPULATION STRUCTURE


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
population_structure$year <- as.integer(as.character(population_structure$year))
population_structure <- population_structure[complete.cases(population_structure),]

rm( population_structure_age_female, population_structure_age_male )

population_structure_max_year = max( population_structure$year )


# Extrapolate the population structure to 2020 by using a linear regression for each age group.
if ( population_structure_max_year < 2020 ) {
  
  missing_years <- data.frame( year = seq(population_structure_max_year+1, 2020, 1) )

  population_structure_extrapolated <- transpose( data.frame(  population_structure %>% 
                                                            filter( year >= population_structure_max_year - 5 ) %>%
                                                            group_by(sex, age) %>%
                                                            group_map( ~ c(  .y$age, .y$sex, coefficients(lm( population_count ~ year, data = .x ))) )))
  
  names(population_structure_extrapolated) <- c("age", "sex", "intercept", "year_coefficient")
  
  population_structure_extrapolated$intercept <- as.double(population_structure_extrapolated$intercept)
  population_structure_extrapolated$year_coefficient <- as.double(population_structure_extrapolated$year_coefficient)
  
  population_structure_extrapolated <- merge( population_structure_extrapolated, missing_years )
  
  population_structure_extrapolated$population_count <- population_structure_extrapolated$intercept + population_structure_extrapolated$year * population_structure_extrapolated$year_coefficient

  population_structure <- rbind( population_structure,
                                 population_structure_extrapolated %>% 
                                   select(age, year, population_count, sex ))
}

population_structure$age <- as.integer(population_structure$age)


# Joining with age group
population_structure <- merge( population_structure, age, by = c("age") )

# If we don't have sex category, we have to transform the population_structure
if ( !has_sex_category ) {
  population_structure <- population_structure %>%
    group_by(age, year, age_group) %>%
    summarise(population_count = sum(population_count))
  population_structure$sex <- 'B'
}


population_structure_by_age_group <-
  population_structure %>%
  group_by( year, sex, age_group)  %>%
  summarise( population_count = sum(population_count))


# Graph: population structure
population_structure %>%
  filter(year==2020) %>%
  ggplot( aes( x = age, y = population_count, color = sex) ) +
  geom_line()  +
  scale_x_continuous(name = "age", breaks = seq(0,100,10), sec.axis = sec_axis( ~ 2020 - ., breaks = seq(1920,2020,10), name = "birth year" ))  +
  this_theme() +
  ggtitle("Population structure in 2020")

append_graph("population_structure")

### MORTALITY

# Load mortality data

if (has_sex_category ){

  mortality_male <- melt(hmd_mx$rate$male, id = c("age"))
  names(mortality_male) <- c("age", "year", "mortality")
  mortality_male$sex <- 'M'
  
  mortality_female <- melt(hmd_mx$rate$female, id = c("age"))
  names(mortality_female) <- c("age", "year", "mortality")
  mortality_female$sex <- 'F'
  
  mortality <- rbind(mortality_female, mortality_male)
  rm(mortality_female)
  rm(mortality_male)
} else {
  mortality <- melt(hmd_mx$rate$total, id = c("age"))
  names(mortality) <- c("age", "year", "mortality")
  mortality$sex <- 'B' 
}


mortality <- 
  mortality %>%
  arrange( age, sex, year ) %>%
  group_by( age, sex ) %>%
  mutate( mortality = na.approx( mortality, na.rm = FALSE ))

mortality$age <- as.integer(as.character(mortality$age))  

mortality <- mortality[complete.cases(mortality),]



# Mortality by age group

mortality_by_age_group <-
  mortality %>%
  merge( population_structure, by = c("year", "sex", "age")) %>%
  group_by( year, sex, age_group ) %>%
  summarize( mortality = sum(mortality*population_count) / sum(population_count))

### MORTALITY GRAPHS


mortality_by_age_group %>%
  filter( age_group >= "50" & year >= 2009) %>%
  ggplot( aes( x = year, y = mortality, color = age_group, linetype = sex) ) +
  geom_line() +
  scale_color_discrete() +
  scale_x_continuous(name = "year", breaks = 2009:2020, minor_breaks = FALSE) +
  scale_y_log10(labels = scales::percent_format(accuracy = 0.1), name = "death rate (log10 scale)")  +
  this_theme() +
  ggtitle("Death rate for different ages")

append_graph("death_rate")

# Compute a liner regression for each age and sex

mortality_lr_coefficients <- transpose( data.frame(  mortality %>% 
                                                       filter( year >=  2009 ) %>%
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

# Graph: expected number of deaths per year vs reality
death_expected_yearly %>%
  filter(year <= 2019) %>%
  group_by(year) %>%
  summarise( death_expected_yearly = sum(death_expected_yearly), death_observed_yearly = sum(death_observed_yearly)) %>%
  ggplot( aes(year, y = value)) + 
  geom_line(aes(y = death_expected_yearly,  color="expected")) + 
  geom_line(aes(y = death_observed_yearly,  color="real")) +
  ggtitle("Expected vs observed number of deaths per year ") +
  scale_x_continuous(name = "year", breaks = 2009:2019, minor_breaks = FALSE, limits = c(2009,2019)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  this_theme( )+ theme(legend.title = element_blank()) 

append_graph("expected_death_per_year")

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



# Graph: expected number of deaths per week vs reality
death_expected_weekly %>%
  filter(year>=2010) %>%
  group_by(date) %>%
  summarise( death_expected = sum(death_expected), death_observed = sum(death_observed)) %>%
  ungroup() %>%
  arrange(date ) %>%
  ggplot( aes(x = date)) + 
  this_theme() + theme(legend.title = element_blank())  +
  geom_line(aes(y = death_expected, col = "expected")) + 
  geom_line(aes(y = death_observed, col = "observed")) +
  scale_x_date( date_breaks = "1 year", labels = date_format("%Y"), name = "year", limits = c( as.Date( "2010-01-01" ), as.Date("2020-12-31") )) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week", limits = c(0,NA)) +
  ggtitle("Expected vs observed number of deaths per week")  

append_graph("expected_death_per_week")

death_expected_weekly %>%
  filter(year>=2020) %>%
  group_by(date) %>%
  summarise( death_expected = sum(death_expected), death_observed = sum(death_observed)) %>%
  ungroup() %>%
  arrange(date ) %>%
  ggplot( aes(x = date)) + 
  this_theme() + theme(legend.title = element_blank())  +
  geom_line(aes(y = death_expected, col = "expected")) + 
  geom_line(aes(y = death_observed, col = "observed")) +
  scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "year", limits = c( as.Date( "2020-01-01" ), Sys.Date() ) ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week", limits = c(0,NA)) +
  ggtitle("Expected vs observed number of deaths per week (since 01/2020)")  

append_graph("expected_death_per_week_2020")

# Cumulative  excess mortality
death_expected_weekly %>%
  arrange(age_group, sex, date)  %>%
  group_by( age_group, sex ) %>%
  mutate( "cum_excess_deaths" = cumsum(excess_death)) %>%
  group_by(date) %>%
  summarise( excess_death = sum(excess_death), 
             cum_excess_deaths = sum(cum_excess_deaths), 
             year = mean(year)) %>%
  ungroup() %>%
  ggplot( aes(x = date)) +
  geom_line(aes(y = excess_death, col = "weekly excess deaths")) + 
  geom_line(aes(y = cum_excess_deaths, col = "cumulative excess deaths")) +
  scale_x_date( date_breaks = "1 year", labels = date_format("%Y"), name = "year") +
  ggtitle("Cumulative excess deaths") +
  this_theme() + theme(legend.title = element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "excess deaths" )

append_graph("cumulative_excess_death_per_year")

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



excess_death_2020 <- excess_death_2020 %>%
  merge( population_structure %>%
           filter(year == 2020) %>%
           group_by( year, sex, age_group)  %>%
           summarise( population_count = sum(population_count)),
         by = c( "age_group", "sex" ) ) %>% 
  merge( death_expected_yearly %>% filter( year == 2020 )) %>% 
  select( -year, -death_observed_yearly ) 

excess_death_2020$excess_death_rate <- excess_death_2020$excess_death / excess_death_2020$population_count
excess_death_2020$expected_death_rate <- excess_death_2020$expected_death  / excess_death_2020$population_count
excess_death_2020$observed_death_rate <- excess_death_2020$death_observed / excess_death_2020$population_count
excess_death_2020$expected_yearly_death_rate <- excess_death_2020$death_expected_yearly / excess_death_2020$population_count
excess_death_2020$observed_yearly_death_rate <- excess_death_2020$expected_yearly_death_rate + excess_death_2020$excess_death_rate



# Graphs 
for ( s in sex) {
  plot <-
    mortality_by_age_group %>%
      filter( age_group >= "50" & sex == s & year >= 2009) %>%
      ggplot( aes( x = year, y = mortality, color = age_group) ) +
      geom_line( ) +
      geom_point(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s), aes( x = 2020, y = observed_yearly_death_rate, color =age_group)) +
      geom_hline(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s), aes( yintercept = observed_yearly_death_rate, color =age_group), linetype="dashed") +
      geom_text(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s), aes( color = age_group, x = 2019.5, y = observed_yearly_death_rate*1.15, label=age_group)) +
      scale_color_discrete() +
      scale_x_continuous(name = "year", limit=c(2009,2020), breaks=2009:2020, minor_breaks = NULL) +
      scale_y_log10(labels = scales::percent_format(accuracy = 0.1), name = "death rate (log10 scale)")  +
      this_theme() +
      ggtitle( sprintf("Historical death rate for different ages\ncompared to projected rate in 2020, sex =", s) )
  
  print(plot)
  
  append_graph(paste("comparative_death_rate", s, sep = "_"))
}


excess_death_2020 %>%
  merge( age_group ) %>%
  filter( age_min >= 45) %>%
  group_by(age_min,sex) %>%
  summarise(expected_death_rate = sum(expected_death) / sum(population_count) ,
            observed_death_rate = sum(death_observed) / sum(population_count) ) %>%
  reshape2::melt(id=c("age_min", "sex")) %>%
  ggplot(aes(x=age_min, y=value, color=sex, linetype=variable)) +
  geom_point(aes()) +
  geom_line(aes()) +
  scale_y_log10(labels = scales::percent_format(accuracy = 0.1), name = "death rate (log10 scale)", limits = c(0.0005, 0.3))  +
  scale_x_continuous(name = "age group") +
  this_theme() +
  ggtitle( sprintf("expected vs observed death rate in first %d weeks of 2020", max_week_2020)) 


append_graph("excess_death_rate_2020")

# Comparable years

years_with_more_mortality <-
  excess_death_2020 %>%
  select( age_group, sex, observed_yearly_death_rate) %>%
  rename( death_rate_2020 = observed_yearly_death_rate ) %>%
  merge( mortality_by_age_group ) %>%
  filter( mortality > death_rate_2020 ) %>%
  group_by( age_group, sex ) %>%
  summarise( last_year_equivalent_death_rate = max(year))


years_with_more_mortality %>%
  filter( age_group >= "40") %>%
  ggplot(aes(x=age_group, y= last_year_equivalent_death_rate, color = sex)) +
  geom_point(stat="identity", aes(shape = sex), alpha = 0.7, size = 5, fill = "transparent")  +
  scale_y_continuous(name = "year", trans="reverse", limits = c(2020,NA), breaks=1900:2020, minor_breaks = FALSE)  +
  this_theme() +
  ggtitle( "Number of equivalent years of death rate regression for different age groups") 


append_graph("year_with_comparable_death_rate")


### MORTALITY VARIANCE

mortality_deviation <-
  mortality_by_age_group %>%
    group_by( year, sex, age_group ) %>%
    summarise( mortality = mean(mortality)) %>%
    group_by( sex, age_group ) %>%
    mutate( mortality_rollmean = rollmedian( mortality, 5, align="center", na.pad=TRUE ))

mortality_deviation$deviation <-
  mortality_deviation$mortality - mortality_deviation$mortality_rollmean
mortality_deviation$relative_deviation <-
  mortality_deviation$mortality / mortality_deviation$mortality_rollmean - 1


years_with_more_mortality <-
  mortality_deviation %>%
    merge( excess_death_2020 ) %>%
    filter( deviation >= excess_death_rate ) %>%
    group_by( age_group, sex ) %>%
    summarise( last_year_equivalent_excess_death_rate = max(year))  %>%
    merge( years_with_more_mortality )



years_with_more_mortality %>%
  filter( age_group >= "40") %>%
  ggplot(aes(x=age_group, y=last_year_equivalent_excess_death_rate, color = sex)) +
  geom_point(stat="identity", aes(shape = sex), alpha = 0.7, size = 5, fill = "transparent")  +
  scale_y_continuous(name = "year", trans="reverse", limits = c(2020,NA), breaks=seq(1900,2020,5), minor_breaks = FALSE)  +
  this_theme() +
  ggtitle( "Number of equivalent years of similar excess death rate for different age groups") 

append_graph("year_with_comparable_excess_death_rate")


### COVID 

if ( !file.exists("./data/owid-covid-data.csv")) {
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "./data/owid-covid-data.csv")
}



covid_data <- read_csv("./data/owid-covid-data.csv",
                       col_types = cols(
                         date = col_date(format = "%Y-%m-%d"),
                         new_tests = col_double(),
                         new_tests_per_thousand = col_double(),
                         total_tests = col_double(),
                         total_tests_per_thousand = col_double(),
                         tests_units = col_double(),
                         new_tests_smoothed = col_double(),
                         new_tests_smoothed_per_thousand = col_double(),
                         positive_rate = col_double(),
                         tests_per_case  = col_double(),
                         total_vaccinations = col_double(),
                         total_vaccinations_per_hundred  = col_double(),
                         icu_patients = col_double(),
                         icu_patients_per_million = col_double(),
                         hosp_patients  = col_double(),
                         hosp_patients_per_million  = col_double(),
                         weekly_hosp_admissions = col_double(),
                         weekly_hosp_admissions_per_million = col_double(),
                         weekly_icu_admissions = col_double(),
                         weekly_icu_admissions_per_million = col_double()
                         ))

covid_death <-
  covid_data %>%
  filter( iso_code == country_iso_code ) %>%
  select( date, new_deaths ) %>%
  rename( covid_death = new_deaths ) %>%
  mutate( week = sprintf("%d-W%02d", year(date), week(date)) ) %>%
  group_by( week ) %>%
  summarise( covid_death = sum(covid_death))


covid_death[is.na(covid_death)] <- 0

# Graph: COVID death vs excess death
death_expected_weekly %>%
  group_by( week ) %>%
  summarise( excess_death = sum(excess_death) ) %>%
  merge( covid_death, all.y = TRUE ) %>%
  merge( weeks ) %>%
  ggplot( aes( x = date )) +
    geom_line( aes( y = covid_death, color = "COVID death" )) +
    geom_line( aes( y = excess_death, color = "excess death" )) +
    scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week") +
    this_theme() +
    ggtitle( "Number of weekly excess death compared to COVID19-attributed death") 

  
append_graph("covid_death")

# Graph: COVID death vs excess death (cumulative)
death_expected_weekly %>%
  group_by( week ) %>%
  summarise( excess_death = sum(excess_death) ) %>%
  merge( covid_death, all.y = TRUE ) %>%
  merge( weeks ) %>%
  arrange( date ) %>%
  mutate( covid_death = cumsum( covid_death ),
          excess_death = cumsum( excess_death) ) %>%
  ggplot( aes( x = date )) +
  geom_line( aes( y = covid_death, color = "COVID death" )) +
  geom_line( aes( y = excess_death, color = "excess death" )) +
  this_theme() +
  scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week") +
  ggtitle( "Number of cumulative excess death compared to COVID19-attributed death") 


append_graph("covid_cumulative_death")

all_graphs[[country]] <- graphs
