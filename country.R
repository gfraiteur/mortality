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
library(ISOcodes )

if ( !exists("current_country_hmd_code")) {
  stop("The 'current_country_hmd_code' variable is not defined.")
}


# Remove everything in the environment but not the data we downloaded from mortality.org.
rm(
  list= setdiff(ls(), c("hmd_mx", "hmd_pop", "current_country_hmd_code",
                        "all_graphs", "selected_countries", "all_excess_death_2020", "all_years_with_more_mortality",
                        "all_life_expectancy", "mobility", "all_death_expected_weekly",
                        "all_covid_death", "all_country_summary"))
  )


# Read countries
countries = read_csv("HMD-countries-codes.csv", col_types = 
                     cols(
                       name = col_character(),
                       code = col_character(),
                       iso_code = col_character()
                     )) %>%
  mutate( iso_code = ifelse( is.na(iso_code), code, iso_code))

names(countries) = c( "country_name","country_hmd_code", "country_iso_code")

countries = 
  countries %>% 
  merge( ISO_3166_1 %>% select( country_iso_code = Alpha_3, country_iso_code_2 = Alpha_2))

current_country_name <- (countries %>% filter( current_country_hmd_code == country_hmd_code ))$country_name
current_country_iso_code <- (countries %>% filter( current_country_hmd_code == country_hmd_code ))$country_iso_code
current_country_iso_code_2 <- (countries %>% filter( current_country_hmd_code == country_hmd_code ))$country_iso_code_2

# Functions to work with graphs
graphs <- list()

if ( !exists("all_graphs")) {
 all_graphs <- list() 
}


this_theme <- function(title) {
  caption = sprintf( "Mortality data available until %s", format(max_date,"%Y-%m-%d")  )
  labs(title = title, subtitle = current_country_name, caption = caption) 
}

signed_sqrt_trans <- trans_new(
  name = "signed sqrt",
  trans = function(x) sign(x) * sqrt(abs(x)),
  inverse = function(x) sign(x) * x * x,
  breaks = breaks_extended()
)

signed_square <- function(x) sign(x) * x^2

append_graph <- function(name) {
  graphs[[name]] <<- last_plot()
}

source("passwords.R", local = TRUE )


directory <- paste("./data/", current_country_hmd_code, sep = "")

if (!dir.exists("./data"))  {
  dir.create("./data")
}
if (!dir.exists(directory))  {
  dir.create(directory)
}



# Generate time dimension (years, weeks)
weeks <- merge( 2005:2021,  1:52, all=TRUE )
names(weeks) <- c("year", "week_of_year")
weeks$week <- sprintf( "%i-W%02d", weeks$year, weeks$week_of_year )
weeks$date <- make_date( weeks$year ) - wday(make_date( weeks$year )) + 2 + (weeks$week_of_year - 1) * 7

### LOAD HMD DATA

has_hmd <- exists("hmd_pop") & exists("hmd_mx")
if ( has_hmd ) {
  has_hmd <- hmd_pop$label == current_country_hmd_code
}

if ( !has_hmd  ) {
  hmd_pop <- hmd.pop(current_country_hmd_code, hmd_username, hmd_password)
  hmd_mx <- hmd.mx(current_country_hmd_code, hmd_username, hmd_password)
}
 

### DEATH

# Load death das.integer(death$AgeInterval)ata

death_filename_zip = "./data/STMFinput.zip"
if ( !file.exists(death_filename_zip)){
  download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", death_filename_zip )
  unzip(death_filename_zip, exdir = "./data")
}

death <-
  read_csv(sprintf("data/%sstmf.csv", current_country_hmd_code), col_types = cols(
      Age = col_integer(),
      Year = col_integer(), 
      Week = col_integer() ))  %>%
  filter( !is.na(Week) & !is.na(Age))


# If we have sex info, remove the data for 'both' gender.
has_sex_category <- 'm' %in% death$Sex
if ( has_sex_category ) {
  sex <- c("M", "F")
  death <- death %>% filter( Sex != 'b')
} else {
  sex <- c("B")
}
  


death <- death %>%
  filter( Year >= 2009) %>%
  select( -Type, -Access )

age_end = 120
# Generate age groups from death file
death$age_min <- as.integer(death$Age)
death$age_max <-  ifelse( death$AgeInterval == "+", age_end, as.integer(death$Age) + as.integer(death$AgeInterval) - 1 )

# Coerce the max age group to be 90+. Some countries (Germany for instance) have 95+ and it makes things more difficult to compare.
death <- death %>%
  mutate(
    age_min = ifelse( age_min >= 90, 90, age_min ),
    age_max = ifelse( age_min >= 90, age_end, age_max )
  )


death$age_group <- ifelse( death$age_max == age_end , paste( death$age_min, "+", sep = ""), sprintf("%02d-%02d", death$age_min, death$age_max)  )


age_group <- death %>%
  group_by( age_min, age_max, age_group ) %>%
  summarise( weeks_count = n_distinct(Year, Week) ) %>% 
  ungroup() %>% 
  arrange(age_min)

# In some countries, we have coarse age groups for temporary data (last two or three weeks). 
# We remove these age groups and the deaths data of all affected weeks.
temporary_age_groups = age_group %>% filter( weeks_count < 12 )

if ( !(current_country_hmd_code %in% c("FRATNP","ITA") ) ) {
  death_invalid_weeks = (death %>%
    filter( age_group %in% temporary_age_groups$age_group ) %>% 
    select(Week) %>%
    distinct())$Week
  
  death_excluded_data = death %>% filter( (Week %in% death_invalid_weeks ) )
  death = death %>% filter( !(Week %in% death_invalid_weeks ) )
  
} else {
  # We don't do this in France because this would remove the age group 00-04,
  # which will be handled by the overlapping_age_group logic below/
}


age_group <- age_group %>% filter( !(age_group %in% temporary_age_groups$age_group ) )


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


# Generate age and age supergroup.
age = data.frame( age = 0:age_end )


age_supergroup =
  data.frame( age_min = c(0, 5, 20, 65, 85),
              age_max = c(4, 19, 64, 84, age_end))

age_supergroup$age_supergroup <- ifelse( age_supergroup$age_max == age_end , paste( age_supergroup$age_min, "+", sep = ""), sprintf("%02d-%02d", age_supergroup$age_min, age_supergroup$age_max)  )


age = sqldf( "select age, age_group, age_supergroup
             from age, age_group, age_supergroup
             where age.age between age_group.age_min and age_group.age_max and
             age.age between age_supergroup.age_min and age_supergroup.age_max")


age_group = sqldf( "select age_group.age_min, age_group.age_max,  age_group, age_supergroup
             from  age_group, age_supergroup
             where age_group.age_min between age_supergroup.age_min and age_supergroup.age_max and
              age_group.age_max between age_supergroup.age_min and age_supergroup.age_max")

if ( max( (age_group %>% group_by(age_group) %>% summarise(count = n() ))$count ) > 1 ) {
  stop("Age groups and age supergroups don't match.")
}
  


# Continue processing the death dataframe

death$week <- sprintf( "%i-W%02d", death$Year, death$Week )  

max_date <- max( ( death %>% merge(weeks)) $date  )


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

if ( FALSE ) {
  death %>%
    filter ( year < 2020 ) %>%
    group_by( year ) %>%
    summarize( death_observed = sum( death_observed )) %>%
    ggplot( aes( x = year, y = death_observed)) + 
    geom_line(aes()) + 
    this_theme("Number of deaths per year") +
    scale_x_continuous( breaks = 2005:2019,name = "year") +
    scale_y_continuous( labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths") 
  
  append_graph("death")
}

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


# Extrapolate the population structure to 2021 by using a linear regression for each age group.
if ( population_structure_max_year < 2021 ) {
  
  population_structure_missing_years <- data.frame( year = seq(population_structure_max_year+1, 2021, 1) )

  population_structure_extrapolated <- transpose( data.frame(  population_structure %>% 
                                                            filter( year >= population_structure_max_year - 5 ) %>%
                                                            group_by(sex, age) %>%
                                                            group_map( ~ c(  .y$age, .y$sex, coefficients(lm( population_count ~ year, data = .x ))) )))
  
  names(population_structure_extrapolated) <- c("age", "sex", "intercept", "year_coefficient")
  
  population_structure_extrapolated$intercept <- as.double(population_structure_extrapolated$intercept)
  population_structure_extrapolated$year_coefficient <- as.double(population_structure_extrapolated$year_coefficient)
  
  population_structure_extrapolated <- merge( population_structure_extrapolated, population_structure_missing_years )
  
  population_structure_extrapolated$population_count <- population_structure_extrapolated$intercept + population_structure_extrapolated$year * population_structure_extrapolated$year_coefficient

  population_structure <- rbind( population_structure,
                                 population_structure_extrapolated %>% 
                                   select(age, year, population_count, sex ))
}

population_structure$age <- as.integer(population_structure$age)

population_axis_scale = as.double(
  population_structure %>% 
  filter( year == 2020 & age >= 65 ) %>% 
  group_by() %>%
  summarise(population_count = sum(population_count)) ) / 100000

population_axis_name = "per 100K inh. over 65 in 2020"

population_axis_transform = eval( parse( text = sprintf("function(x) x / %f", population_axis_scale ), keep.source = TRUE ))

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
  this_theme("Population structure in 2020")

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




# Compute a liner regression for each age and sex
mortality_lr_coefficients <- transpose( data.frame(  mortality %>% 
                                                       filter( year >=  2009 ) %>%
                                                       group_by(sex, age) %>%
                                                       group_map( ~ c( .y$sex, .y$age, coefficients(lm( mortality ~ year, data = .x ))) )))

names(mortality_lr_coefficients) <- c("sex", "age", "intercept_coeff", "year_coeff")
mortality_lr_coefficients$intercept_coeff <- as.double(mortality_lr_coefficients$intercept_coeff)
mortality_lr_coefficients$year_coeff <- as.double(mortality_lr_coefficients$year_coeff)

# Remove NA. We should have them only for high age with very low number of people so it is safe to ignore.
mortality_lr_coefficients = mortality_lr_coefficients[complete.cases(mortality_lr_coefficients), ]

# Extend the mortality dataset with projections
mortality$extrapolated = FALSE
mortality_max_year = max(mortality$year)
if ( mortality_max_year < 2019 ) {
  
  mortality_missing_years <- data.frame( year = seq(mortality_max_year+1, 2019, 1) )
  
  

  mortality_extrapolated <- merge( mortality_lr_coefficients, mortality_missing_years )
  
  mortality_extrapolated$mortality <- mortality_extrapolated$intercept_coeff + mortality_extrapolated$year * mortality_extrapolated$year_coeff
  mortality_extrapolated$extrapolated = TRUE
  mortality_extrapolated$age = as.integer(mortality_extrapolated$age)
  
  mortality <- rbind( mortality  %>% select(age, sex, year, mortality, extrapolated ),
                      mortality_extrapolated %>% select(age, sex, year, mortality, extrapolated ))
}


# Mortality by age group

mortality_by_age_group <-
  mortality %>%
  merge( population_structure, by = c("year", "sex", "age")) %>%
  group_by( year, sex, age_group ) %>%
  summarize( mortality = sum(mortality*population_count) / sum(population_count))

### MORTALITY GRAPHS

if (FALSE) {
  mortality_by_age_group %>%
    filter( age_group >= "50" & year >= 2009) %>%
    ggplot( aes( x = year, y = mortality, color = age_group, linetype = sex) ) +
    geom_line() +
    scale_color_discrete() +
    scale_x_continuous(name = "year", breaks = 2009:2020, minor_breaks = FALSE) +
    scale_y_log10(labels = scales::percent_format(accuracy = 0.1), name = "death rate (log10 scale)")  +
    this_theme("Death rate for different ages")
  
  append_graph("death_rate")
}


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
  filter(year <= year(max_date)) %>%
  group_by(year) %>%
  summarise( death_expected_yearly = sum(death_expected_yearly), death_observed_yearly = sum(death_observed_yearly)) %>%
  ggplot( aes(x = year, y = value)) + 
  geom_line(aes(y = death_expected_yearly,  color="expected")) + 
  geom_line(aes(y = death_observed_yearly,  color="real")) +
  this_theme("Expected vs observed number of deaths per year ") +
  scale_x_continuous(name = "year", breaks = 2009:2020, minor_breaks = FALSE, limits = c(2009,2020)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     limits = c(3000 * population_axis_scale, 8500 * population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name)) +
  theme(legend.title = element_blank()) 

append_graph("expected_death_per_year")

# Graph: patterns of weeks of year
death_per_week_of_year %>%
  merge( death_expected_yearly %>% filter(year==2020) ) %>%
  mutate( death_expected = week_death_percent * death_expected_yearly ) %>%
  group_by(week_of_year,age_group) %>%
  summarise(death_expected=sum(death_expected)) %>%
  ggplot(aes(x=week_of_year,y=death_expected, fill=forcats::fct_rev(age_group))) +
  geom_area( stat="identity" ) +
  scale_fill_discrete(name = "age group" ) + 
  this_theme("Week-of-year mortality pattern projected on the 2020 population")   
  
append_graph("death_per_week_of_year")

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
   theme(legend.title = element_blank())  +
  geom_line(aes(y = death_expected, col = "expected")) + 
  geom_line(aes(y = death_observed, col = "observed")) +
  scale_x_date( date_breaks = "1 year", labels = date_format("%Y"), name = "year", limits = c( as.Date( "2010-01-01" ), Sys.Date() )) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week", 
                     limits = c(0,320 * population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name)) +
  this_theme("Expected vs observed number of deaths per week")  

append_graph("expected_death_per_week")

death_expected_weekly %>%
  filter(year>=2020) %>%
  group_by(date) %>%
  summarise( death_expected = sum(death_expected), death_observed = sum(death_observed)) %>%
  ungroup() %>%
  arrange(date ) %>%
  ggplot( aes(x = date)) + 
   theme(legend.title = element_blank())  +
  geom_line(aes(y = death_expected, col = "expected")) + 
  geom_line(aes(y = death_observed, col = "observed")) +
  scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "year", limits = c( as.Date( "2020-01-01" ), Sys.Date() ) ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week", 
                     limits = c(0,320 * population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name)) +
  this_theme("Expected vs observed number of deaths per week (since 01/2020)")  

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
  scale_x_date( date_breaks = "1 year", labels = date_format("%Y"), name = "year", limits = c( as.Date( "2009-01-01" ), Sys.Date() )) +
  this_theme("Cumulative excess deaths") +
   theme(legend.title = element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "excess deaths",
                     limits = c(-500*population_axis_scale, 1500 * population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name))

append_graph("cumulative_excess_death_per_year")

# Table: excess mortality in 2020
excess_death_2020 =
  death_expected_weekly %>%
    filter(date >= as.Date("2020-03-01")) %>% 
    group_by( age_group,sex ) %>%
    summarise( 
               expected_death = sum(death_expected), 
               excess_death = sum(excess_death), 
               death_observed = sum(death_observed),
               excess_death_percent = sum(excess_death) / sum(death_expected))



excess_death_2020 =
  excess_death_2020 %>%
  merge( population_structure %>%
           filter(year == 2020) %>%
           group_by( year, sex, age_group)  %>%
           summarise( population_count = sum(population_count)),
         by = c( "age_group", "sex" ) ) %>% 
  select( -year ) 

excess_death_2020$excess_death_rate <- excess_death_2020$excess_death / excess_death_2020$population_count
excess_death_2020$expected_death_rate <- excess_death_2020$expected_death  / excess_death_2020$population_count
excess_death_2020$observed_death_rate <- excess_death_2020$death_observed / excess_death_2020$population_count


# Comparable years: this info cannot be computed because we are no longer computing mortality for a year, but but a longer period.
if ( FALSE ){
  
  # Test whether the excess death is significant
  if ( current_country_hmd_code != 'ISL') {
    excess_death_2020$excess_death_signicant = unlist(Map(function(x,n,p) prop.test(x,n,p,alternative="greater")$p.value < 0.05, 
                                           x = excess_death_2020$death_observed, 
                                           n = excess_death_2020$population_count, 
                                           p = excess_death_2020$expected_death_rate))
  } else {
    # The above test fails for Island.
    excess_death_2020$excess_death_signicant = FALSE
  }

  
  years_with_more_mortality <-
    excess_death_2020 %>%
    select( age_group, sex, observed_yearly_death_rate, excess_death_signicant) %>%
    rename( death_rate_2020 = observed_yearly_death_rate ) %>%
    merge( mortality_by_age_group ) %>%
    filter( mortality > death_rate_2020 ) %>%
    group_by( age_group, sex, excess_death_signicant ) %>%
    summarise( last_year_equivalent_death_rate = max(year)) %>%
    ungroup() %>%
    mutate( death_rate_regression = ifelse( !excess_death_signicant, 0, 2020-last_year_equivalent_death_rate) )
  
  
  years_with_more_mortality %>%
    ggplot(aes(x=age_group, y= death_rate_regression, fill = sex)) +
    geom_bar(stat="identity",position = position_dodge2(preserve = "single"))  +
    geom_hline(yintercept = 2010) +
    scale_y_sqrt(name = "number of years of regression (square root scale)",  limits = c(0,100), breaks= c( 1, seq(0,10,2), seq(15,50,5), seq(50,100,10) ) , minor_breaks = FALSE,
                 sec.axis = sec_axis( trans = function(y) 2020-y, breaks= 2020 - c( 1, seq(0,10,2), seq(15,50,5), seq(50,100,10) ), name = "equivalent year" )  )  +
    this_theme( "Exceptionality of the 2020 absolute mortality rates") 
  
  
  append_graph("year_with_comparable_death_rate")
  
  
  # Graphs 
  for ( s in sex) {
    plot <-
      mortality_by_age_group %>%
        filter( age_group >= "50" & sex == s & year >= 2000) %>%
        ggplot( aes( x = year, y = mortality, color = age_group) ) +
        geom_line( ) +
        geom_point(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s), 
                   aes( x = 2020, y = observed_yearly_death_rate, color =age_group)) +
        geom_point(data = merge(excess_death_2020,years_with_more_mortality) %>% filter( age_group >= "50" & sex == s), 
                   aes( x = last_year_equivalent_death_rate, y = observed_yearly_death_rate, color =age_group)) +
        geom_hline(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s), 
                   aes( yintercept = observed_yearly_death_rate, color =age_group), linetype="dashed") +
        geom_text(data = excess_death_2020 %>% filter( age_group >= "50" & sex == s),
                  aes( color = age_group, x = 2019.5, y = observed_yearly_death_rate*1.15, label=age_group)) +
        scale_color_discrete() +
        scale_x_continuous(name = "year", limit=c(2000,2020), breaks=2000:2020, minor_breaks = NULL) +
        scale_y_log10(labels = scales::percent_format(accuracy = 0.1), name = "death rate (log10 scale)", limits = c(0.001, 0.4))  +
        this_theme( sprintf("Historical death rate for different ages\ncompared to extrapolated rate in 2020, sex = %s", s) )
    
    print(plot)
    
    append_graph(paste("comparative_death_rate", s, sep = "_"))
  }

}


excess_death_2020 %>%
  merge( age_group ) %>%
#  filter( age_min >= 45) %>%
  group_by(age_min,sex) %>%
  summarise(expected_death_rate = sum(expected_death) / sum(population_count) ,
            observed_death_rate = sum(death_observed) / sum(population_count) ) %>%
  reshape2::melt(id=c("age_min", "sex")) %>%
  ggplot(aes(x=age_min, y=value, color=sex, linetype=variable)) +
  geom_point(aes()) +
  geom_line(aes()) +
  scale_y_log10(labels = scales::percent_format(accuracy = 0.01), name = "death rate (log10 scale)", limits = c(0.000025, 0.5))  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), limits= c(0,100), minor_breaks = FALSE) +
  this_theme( "Expected vs observed mortality rate since March 2020") 

append_graph("compared_death_rate_2020")

excess_death_2020 %>%
  merge( age_group ) %>%
  filter( age_min >= 40) %>%
  group_by(age_min,sex) %>%
  summarise(excess_death_rate = (sum(death_observed) - sum(expected_death)) / sum(population_count)  )  %>%
  ggplot(aes(x=age_min, y=excess_death_rate, color=sex)) +
  geom_point(aes()) +
  geom_line(aes()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), name = "excess death rate", limits = c(-0.04, 0.1), breaks = seq(-0.04,0.1,0.01))  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), minor_breaks = FALSE) +
  this_theme( "Excess mortality rate since March 2020") 

append_graph("excess_death_rate_2020")

excess_death_2020 %>%
  merge( age_group ) %>%
  group_by(age_min,sex) %>%
  summarise(excess_death_rate = (sum(death_observed) / sum(expected_death)) -1  )  %>%
  ggplot(aes(x=age_min, y=excess_death_rate, color=sex, fill = sex)) +
  geom_point(aes()) +
  geom_smooth(se=FALSE, span=0.4 ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "relative excess death rate (square root scale)",
                     limits = c(-1, 1.25), breaks = signed_square(seq(-1,2,0.1) ), trans = signed_sqrt_trans)  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), minor_breaks = FALSE) +
  this_theme( "Relative excess death rate 2020") 


append_graph("relative_excess_death_rate_2020")

life_expectancy = data.frame( age = (age %>% filter(age <= 100))$age ) %>% merge( c("male", "female")) %>% rename( sex = y )
life_expectancy$life_expectancy = unlist(Map(function(a,s) flife.expectancy(hmd_mx, series = s, years = c(mortality_max_year), age = a,  PI = TRUE)[1], a = life_expectancy$age, s =  life_expectancy$sex))
life_expectancy = life_expectancy %>% merge( age %>% merge( c("male", "female")) %>% rename( sex = y ), all.y = TRUE)
life_expectancy[is.na(life_expectancy)] = 2
life_expectancy$sex = ifelse( life_expectancy$sex == "male", "M", "F")


life_expectancy %>%
  group_by( sex, age_group ) %>%
  summarise( life_expectancy = mean(life_expectancy)) %>%
  merge(excess_death_2020) %>%
  mutate( lost_days = 365 * excess_death * life_expectancy / population_count) %>%
  merge( age_group ) %>%
  ggplot( aes(x=age_min, y=lost_days, color=sex, fill = sex)) +
    geom_point() +
    geom_smooth(se=FALSE, span = 0.4, method = "loess" ) +
    this_theme( "Number of lost days of life expectancy per person according to age group since March 2020")  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "days",
                     limits = c(-20, 70))
  
append_graph("lost_life_expectancy")



### MORTALITY VARIANCE

if ( FALSE ) {
  
  
  mortality_deviation <-
    mortality_by_age_group %>%
      group_by( year, sex, age_group ) %>%
      summarise( mortality = mean(mortality)) %>%
      group_by( sex, age_group ) %>%
      mutate( mortality_rollmean = rollapply( mortality, 3, function(z) (z[1] + z[3])/2, align="center", fill = NA  ) )
  
  mortality_deviation$deviation <-
    mortality_deviation$mortality - mortality_deviation$mortality_rollmean
  mortality_deviation$relative_deviation <-
    mortality_deviation$mortality / mortality_deviation$mortality_rollmean - 1
  
  
  years_with_more_mortality <-
    mortality_deviation %>%
      merge( excess_death_2020 ) %>%
      filter( deviation >= excess_death_rate ) %>%
      group_by( age_group, sex, excess_death_signicant ) %>%
      summarise( last_year_equivalent_excess_death_rate = max(year))  %>%
      ungroup() %>%
      mutate( excess_death_rate_regression = ifelse( !excess_death_signicant, 0,  2020-last_year_equivalent_excess_death_rate)) %>%
      merge( years_with_more_mortality )
  
  
  
  years_with_more_mortality %>%
    ggplot(aes(x=age_group, y=excess_death_rate_regression, fill = sex)) +
    geom_bar(stat="identity", position = position_dodge2(preserve = "single"))  +
    geom_hline(yintercept = 2010) +
    scale_y_sqrt(name = "number of years of regression (square root scale)",  limits = c(0,100), breaks= c( 1, seq(0,10,2), seq(15,50,5), seq(50,100,10) ) , minor_breaks = FALSE,
                 sec.axis = sec_axis( trans = function(y) 2020-y, breaks= 2020 - c( 1, seq(0,10,2), seq(15,50,5), seq(50,100,10) ), name = "equivalent year" )  )  +
    this_theme( "Exceptionality of the 2020 excess mortality rates")
  
  append_graph("year_with_comparable_excess_death_rate")

}

### COVID 

if ( !file.exists("./data/owid-covid-data.csv")) {
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "./data/owid-covid-data.csv")
}



if ( !exists("all_covid_death")) {
all_covid_death <- read_csv("./data/owid-covid-data.csv",
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
                         )) %>%
  filter( !is.na( iso_code )) %>%
  select( date, new_deaths, iso_code ) %>%
  rename( covid_death = new_deaths, country_iso_code = iso_code ) %>%
  mutate( week = sprintf("%d-W%02d", year(date), week(date)) ) %>%
  group_by( country_iso_code, week ) %>%
  summarise( covid_death = sum(covid_death, na.rm = TRUE))

  all_covid_death[is.na(all_covid_death)] <- 0
}

covid_death = all_covid_death %>%
  filter( country_iso_code == current_country_iso_code )



# Graph: COVID death vs excess death
death_expected_weekly %>%
  group_by( week ) %>%
  summarise( excess_death = sum(excess_death)  ) %>%
  merge( covid_death, all.y = TRUE ) %>%
  merge( weeks ) %>%
  ggplot( aes( x = date )) +
    geom_line( aes( y = covid_death, color = "COVID death" )) +
    geom_line( aes( y = excess_death, color = "excess death" )) +
    scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "deaths per week",
                       limits = c(-50*population_axis_scale, 200*population_axis_scale),
                       sec.axis = sec_axis( population_axis_transform, name = population_axis_name)
                       ) +
    this_theme( "Number of weekly excess deaths compared to COVID19-attributed deaths") 

  
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
  scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "cumulative number of deaths",
                     limits = c(-200*population_axis_scale, 2000*population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name)) +
  this_theme( "Number of cumulative excess deaths compared to COVID19-attributed deaths") 


append_graph("covid_cumulative_death")



###  Covid-19 government response stringency index

if ( !file.exists("./data/OxCGRT_latest.csv")) {
  download.file("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", "./data/OxCGRT_latest.csv")
}

restrictions <- read_csv("data/OxCGRT_latest.csv", 
                         col_types = cols(CountryName = col_skip(), 
                                          RegionName = col_skip(), RegionCode = col_skip(), 
                                          Jurisdiction = col_skip(), Date = col_integer()))

restrictions = restrictions %>%
  merge( countries, by.x = c("CountryCode"), by.y = c("country_iso_code"), suffixes = c("","") ) %>%
  rename( country_iso_code = CountryCode ) %>%
  mutate( year = floor(Date/10000), month = floor( ( Date %% 10000 ) / 100  ), day = Date %% 100 ) %>% 
  mutate( date = make_date( year, month, day)) %>%
  select( -Date, -year, -month, -day) %>%
  complete( country_iso_code, date ) %>%
  mutate( week =date - wday(make_date( date )) - 1 )


restrictions %>%  
  filter( country_hmd_code == current_country_hmd_code ) %>%
  ggplot(aes(x=date, y=StringencyIndex, color = country_iso_code )) +
  geom_line() +
    scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date", limits = c(as.Date("2020-03-01"),  Sys.Date())) +
    scale_y_continuous(limits = c(0, 100)) +
    this_theme( "Government response stringency")
  
append_graph("stringency")


## Google mobility data
if (!exists("mobility")) {
  if ( !file.exists("./data/Global_Mobility_Report.csv")){
    download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", "./data/Global_Mobility_Report.csv")
  }
  
  mobility <- read_csv("data/Global_Mobility_Report.csv", 
                       col_types = cols(country_region = col_skip(), 
                                        #sub_region_1 = col_skip(), 
                                        sub_region_2 = col_skip(), 
                                        metro_area = col_skip(), iso_3166_2_code = col_skip(), 
                                        census_fips_code = col_skip()))  %>%
            filter( is.na(sub_region_1) ) %>%
            rename(  retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
                     grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
                     transit_stations = transit_stations_percent_change_from_baseline,
                     workplaces = workplaces_percent_change_from_baseline,
                     residential = residential_percent_change_from_baseline,
                     parks = parks_percent_change_from_baseline) %>%
            mutate(  week = make_date(year(date)) - wday(make_date( year(date))) + 2 +  7* week(date))   %>%
            group_by( week, country_region_code ) %>%
            select( -date ) %>%
            rename( date = week, country_iso_code_2 = country_region_code ) %>%
            summarise( retail_and_recreation = mean(retail_and_recreation, na.rm = TRUE ),
                         grocery_and_pharmacy = mean(grocery_and_pharmacy, na.rm = TRUE ),
                         transit_stations = mean(transit_stations, na.rm = TRUE ),
                         workplaces = mean(workplaces, na.rm = TRUE ),
                       residential = mean(residential, na.rm = TRUE ) ,
                       parks = mean(parks, na.rm = TRUE ) 
                       ) %>%
              ungroup()
              
              
}

mobility %>% 
  filter ( country_iso_code_2 == current_country_iso_code_2 ) %>%
  select( date, workplaces, transit_stations, retail_and_recreation ) %>%
  melt("date") %>%
  ggplot(aes(x=date, y = value, color = variable ))  +
    geom_line() +
    scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date", limits = c(as.Date("2020-03-01"), Sys.Date())) +
    scale_y_continuous(limits = c(NA, NA), name = "percent changes from baseline") +
    this_theme( "Mobility index by Google") 



append_graph("mobility")

mobility %>% 
  filter ( country_iso_code_2 == current_country_iso_code_2 ) %>%
  merge( restrictions %>% filter( country_hmd_code == current_country_hmd_code ) ) %>%
  merge( 
    death_expected_weekly %>%
      group_by( date ) %>%
      summarise( excess_death = sum(excess_death), death_expected = sum(death_expected)) %>%
      mutate( mortality_increase = 100 * (excess_death /  death_expected)  ),
      all = TRUE
    ) %>%
  mutate( mobility_decrease = -(workplaces + transit_stations + retail_and_recreation) / 3 ) %>%
  mutate( retail_and_recreation_decrease = -retail_and_recreation ) %>%
  select( date, StringencyIndex,  mobility_decrease, mortality_increase ) %>%
  rename( government_stringency_index = StringencyIndex ) %>%
  melt("date") %>%
    ggplot(aes(x=date, y = value, color = variable ))  +
    geom_line() +
    guides(linetype = FALSE)  +
    scale_y_continuous(limits = c(-100, 160), name = "") +
    scale_x_date( date_breaks = "1 month", labels = date_format("%m/%y"), name = "date", limits = c(as.Date("2020-03-01"),  Sys.Date())) +
    this_theme( "Government restrictions with effect on reduction of mobility") 
  

  
  
append_graph("restrictions_mobility")

country_summary = data.frame( country_hmd_code = c(current_country_hmd_code), max_date = c(max_date))


# All-country aggregations

all_graphs[[current_country_hmd_code]] <- graphs


if ( !exists("all_country_summary")) {
  all_country_summary <- country_summary
} else {
  all_country_summary <- rbind( all_country_summary, country_summary )
}


excess_death_2020$country_hmd_code = current_country_hmd_code
if ( !exists("all_excess_death_2020")) {
  all_excess_death_2020 <- excess_death_2020
} else {
  all_excess_death_2020 <- rbind( all_excess_death_2020, excess_death_2020 )
}

if ( FALSE ) {
  years_with_more_mortality$country_hmd_code = current_country_hmd_code
  
  if ( !exists("all_years_with_more_mortality")) {
    all_years_with_more_mortality <- years_with_more_mortality
  } else {
    all_years_with_more_mortality <- rbind( all_years_with_more_mortality, years_with_more_mortality )
  }
}

life_expectancy$country_hmd_code = current_country_hmd_code
if ( !exists("all_life_expectancy")) {
  all_life_expectancy <- life_expectancy
} else {
  all_life_expectancy <- rbind( all_life_expectancy, life_expectancy )
}

death_expected_weekly$country_hmd_code = current_country_hmd_code
if ( !exists("all_death_expected_weekly")) {
  all_death_expected_weekly <- death_expected_weekly
} else {
  all_death_expected_weekly <- rbind( all_death_expected_weekly, death_expected_weekly )
}

