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
library(ggpubr)
library(dplyr)
library(GGally)

selected_countries = c( "ESP", "BEL", "POL", "NLD", "ITA",
                        "CZE", "FRATNP", "CHE", "DNK", "DEUTNP", 
                        "FIN", "SWE", "GRC", "HUN", "HRV", "EST",
                        "SVN", "LUX", "ISL", "BGR", "LTU", "SVK" )

# RUS: no data for 2020.  

if ( exists("all_graphs")) rm( all_graphs )
if ( exists("all_excess_death_2020")) rm( all_excess_death_2020 )
if ( exists("all_years_with_more_mortality")) rm( all_years_with_more_mortality )
if ( exists("all_life_expactancy")) rm( all_life_expactancy )
if ( exists("all_covid_death")) rm( all_covid_death )
if ( exists("all_death_expected_weekly")) rm( all_death_expected_weekly )
if ( exists("all_country_summary")) rm( all_country_summary )

# file.remove("./data/all_countries.Rdata")
if ( !file.exists("./data/all_countries.Rdata")) {
  
  for ( current_country_hmd_code in selected_countries ) {
    source("country.R")
  }
  
  save.image("./data/all_countries.Rdata")
  
} else {
  load("./data/all_countries.Rdata")
}



selected_countries_sorted = 
  all_excess_death_2020 %>%
    group_by( country_hmd_code ) %>%
    summarise( excess_death_percent = sum( excess_death ) / sum(expected_death  )) %>%
    arrange( excess_death_percent ) %>%
    select(country_hmd_code )


selected_countries_info =
  selected_countries_sorted %>%
  merge( countries, by.x = c(1), by.y = "country_hmd_code" ) 
  
  

rownames(selected_countries_info) <- selected_countries_info$country_hmd_code

all_graphs_by_kind = list()

for ( country in selected_countries_sorted$country_hmd_code) {
  country_graphs = all_graphs[[country]]
  for ( graph_name in names(country_graphs) ) {
    if ( is.null( all_graphs_by_kind[[graph_name]] ) ) {
      all_graphs_by_kind[[graph_name]] = list()
      all_graphs_by_kind[[graph_name]][["title"]] = all_graphs[[country]][[graph_name]]$labels$title  
    }
    
    all_graphs_by_kind[[graph_name]][[country]] = all_graphs[[country]][[graph_name]]
    all_graphs_by_kind[[graph_name]][[country]]$labels$title = NULL
  }
}

print_graphs = function(graphs) {
  graphs_without_title = graphs
  graphs_without_title$title = NULL
  figure <- ggarrange( plotlist = graphs_without_title,
                       ncol = 3,
                       nrow = 8,
                       common.legend = TRUE, 
                       legend = "top" 
                       )
  figure <- annotate_figure(figure, top = text_grob(trimws( graphs$title ), size = 18 ) )
  
  print(figure)
  
}

if ( FALSE ) {

all_death_score = 
  all_years_with_more_mortality %>%
    group_by( country_hmd_code ) %>%
    summarise( excess_death_rate_regression = mean(excess_death_rate_regression), 
               death_rate_regression = mean(death_rate_regression)) %>%
    ungroup() %>%
    merge( all_excess_death_2020 %>%
       group_by( country_hmd_code ) %>%
         summarise( death_increase = (sum(death_observed) / sum(expected_death )) - 1 )  ) %>%
  group_by( ) %>%
  mutate( max_excess_death_rate_regression = max(excess_death_rate_regression), 
          max_death_rate_regression = max(death_rate_regression),
          max_death_increase = max(death_increase)) %>%
  ungroup() %>%
  mutate( normalized_excess_death_rate_regression = excess_death_rate_regression / max_excess_death_rate_regression,
          normalized_death_rate_regression =  death_rate_regression / max_death_rate_regression,
          normalized_death_increase =  death_increase / max_death_increase ) %>%
  mutate( total_score = normalized_excess_death_rate_regression + 
            normalized_death_rate_regression + 
            normalized_death_increase  ) %>%
  arrange( total_score ) %>%
  select( -max_excess_death_rate_regression, -max_death_rate_regression, -max_death_increase)
  
}


restriction_summary =
  restrictions %>%
    filter( year(date) == 2020 ) %>%
    filter( country_hmd_code %in% selected_countries ) %>%
     group_by( country_hmd_code, country_iso_code ) %>%
     summarise( stringency = mean(StringencyIndex, na.rm = TRUE),
                government_response = mean(GovernmentResponseIndex, na.rm = TRUE),
                containment_health = mean(ContainmentHealthIndex, na.rm = TRUE),
                economic_support = mean(EconomicSupportIndex, na.rm = TRUE ),
                )


mobility_summary =
  mobility %>%
  filter( year(date) == 2020 ) %>%
  filter( country_iso_code_2 %in% selected_countries_info$country_iso_code_2 ) %>%
  group_by( country_iso_code_2 ) %>%
  summarise( mobility_index = mean( retail_and_recreation + transit_stations + workplaces ) / 3,
             retail_and_recreation = mean(retail_and_recreation),
             transit_stations = mean(transit_stations),
             workplaces = mean(workplaces),
             parks = mean(parks),
             residential = mean(residential))



# Mortality by age groups

all_mortality_by_age_supergroup =
  all_death_expected_weekly %>%
    filter( date > as.Date("2020-03-01") & date < as.Date("2020-12-31")) %>%
    merge(age_group) %>%
    group_by( country_hmd_code, age_supergroup ) %>%
    summarise( excess_death_percent =  (sum(death_observed) / sum(death_expected)) - 1 )


all_mortality_by_age_supergroup_dcast =
  all_mortality_by_age_supergroup %>%
    dcast( country_hmd_code ~ age_supergroup, value.var="excess_death_percent" ) %>%
    rename ( excess_death_infant = "00-04",
             excess_death_youth = "05-19",
             excess_death_active = "20-64", 
             excess_death_retired = "65-84",
             excess_death_elder = "85+")

big_correlation = 
  all_mortality_by_age_supergroup %>%
    merge( countries ) %>%
    merge( all_country_summary ) %>%
    #filter( max_week_2020 == 53 ) %>%
    merge(mobility_summary) %>%
    merge( all_covid_death %>% 
             filter( country_iso_code %in% selected_countries_info$country_iso_code) %>% 
             group_by( country_iso_code) %>% 
             summarise(covid_death_count=sum(covid_death))) %>%
            merge(
              all_death_expected_weekly %>%
                filter( year == 2020 ) %>%
                group_by( country_hmd_code ) %>%
                summarise( death_expected = sum(death_expected))
            ) %>%
    merge( all_death_expected_weekly  %>%
             merge(age_group) %>%
             group_by(country_hmd_code, age_supergroup ) %>%
             summarise( excess_death_rate = sum(death_observed) / sum(death_expected))
           ) %>%
    mutate( covid_death_percent = covid_death_count / death_expected ) 
  


all_youth_mortality  =
  all_excess_death_2020 %>%
    merge( age_group ) %>%
    mutate( age_group = if_else( age_min < 65, "over_65", "under_65") ) %>%
    group_by( country_hmd_code, age_group) %>%
    summarise( excess_death_percent = sum(excess_death) / sum(expected_death)) %>%
    dcast(   country_hmd_code ~ age_group, value.var="excess_death_percent" )
 



# Latency between excess mortality peak and stringency index

# 
# country_peaks =
#   restrictions %>%
#     filter( country_hmd_code %in% selected_countries & StringencyIndex >= 60) %>%
#     group_by( country_hmd_code ) %>%
#     summarise( restriction_date = min(date), max_restrictions = max(StringencyIndex)) %>%
#     merge(
#       TODO
#       ) %>%
#     merge( countries ) %>%
#     merge( 
#       all_covid_death %>%
#         filter( country_iso_code %in% selected_countries_info$country_iso_code) %>%
#         merge(weeks) %>%
#         filter( date > as.Date("2020-03-01") & date < as.Date("2020-07-01")) %>%
#         group_by( country_iso_code ) %>%
#         summarise( death_peak_date = date[which.max(covid_death)] )
#       ) %>%
#     mutate( death_peak_delay = as.integer( death_peak_date - restriction_date ) ) 
# 
# 
# country_peaks %>%
#   merge(all_death_score) %>%
#   select( max_restrictions, death_peak_delay, death_rate_regression, excess_death_infant, excess_death_youth,excess_death_active,  excess_death_retired,excess_death_elder  ) %>%
#   ggpairs(  )
# 




