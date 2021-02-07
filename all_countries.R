library(ggpubr)
library(GGally)

selected_countries = c( "ESP", "BEL", "POL", "NLD", "ITA", "CZE", "FRATNP", "CHE", "DNK", "DEUTNP", "FIN", "SWE", "GRC", "HUN", "HRV", "EST", "SVN" )

if ( exists("all_graphs")) rm( all_graphs )
if ( exists("all_excess_death_2020")) rm( all_excess_death_2020 )
if ( exists("all_years_with_more_mortality")) rm( all_years_with_more_mortality )
if ( exists("all_life_expactancy")) rm( all_life_expactancy )
if ( exists("all_covid_death")) rm( all_covid_death )
if ( exists("all_death_expected_weekly")) rm( all_death_expected_weekly )
if ( exists("all_country_summary")) rm( all_country_summary )


for ( current_country_hmd_code in selected_countries ) {
  source("country.R")
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
                       nrow = 6,
                       common.legend = TRUE, 
                       legend = "top" 
                       )
  figure <- annotate_figure(figure, top = text_grob(trimws( graphs$title ), size = 18 ) )
  
  print(figure)
  
}



all_excess_death_2020 %>%
  merge( age_group ) %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  group_by( age_min,sex) %>%
  summarise(excess_death_rate = (sum(death_observed) / sum(expected_death)) -1  )  %>%
  ggplot(aes(x=age_min, y=excess_death_rate, color=sex, fill = sex)) +
  geom_point(aes()) +
  geom_smooth(se = FALSE, span = 0.4) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "relative excess death rate (square root scale)",
                     limits = c(-1, 1.25), breaks = signed_square(seq(-1,2,0.1) ), trans = signed_sqrt_trans)  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), minor_breaks = FALSE) +
  labs( title = "Relative excess death rate 2020 (all countries)") 

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
  



all_death_score %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  select(country_hmd_code, normalized_death_rate_regression,  normalized_excess_death_rate_regression, normalized_death_increase ) %>%
  melt() %>%
    ggplot(aes(x=country_hmd_code, y=value, fill=variable)) +
    geom_col(position=position_stack())  +
    scale_x_discrete(limits=all_death_score$country_hmd_code)




all_life_expectancy %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  group_by( sex, age_group, country_hmd_code ) %>%
  summarise( life_expectancy = mean(life_expectancy)) %>%
  merge(all_excess_death_2020) %>%
  mutate( lost_days = 365 * excess_death * life_expectancy / population_count ) %>%
  merge( age_group ) %>%
  ggplot( aes(x=age_min, y=lost_days, color=sex, fill = sex)) +
  geom_boxplot(aes(x=reorder(age_min,sex))) +
  ggtitle( "Number of lost years of life expectancy in 2020")  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "days") 



restriction_summary =
  restrictions %>%
    filter( year(date) == 2020 ) %>%
    filter( country_hmd_code %in% selected_countries ) %>%
     group_by( country_hmd_code, country_iso_code ) %>%
     summarise( stringency = mean(StringencyIndex, na.rm = TRUE),
                government_response = mean(GovernmentResponseIndex, na.rm = TRUE),
                containment_health = mean(ContainmentHealthIndex, na.rm = TRUE),
                economic_support = mean(EconomicSupportIndex, na.rm = TRUE )
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


# Mortality regression by stringency index (most significant correlation)
restriction_summary %>%
  merge(all_death_score) %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  ggplot(aes(x=death_rate_regression, y=stringency, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.1, nudge_y = 1 ) +
  xlab("mortality rate regression (years)") + ylab("government stringency") 

# Mortality by containment health index
restriction_summary %>%
  merge(all_death_score) %>%
  ggplot(aes(x=death_increase, y=containment_health, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.005, nudge_y = 1 ) +
  xlab("excess mortality (%)") + ylab("containment index") 


mobility_summary %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  merge( countries ) %>%
  merge(all_death_score) %>%
  ggplot(aes(x=death_increase, y=retail_and_recreation, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.005, nudge_y = 1.5, alpha = 0.5 ) +
  xlab("excess mortality score") + ylab("mobility decrease") 


# Mortality by age groups

all_mortality_by_age_supergroup =
  all_death_expected_weekly %>%
    filter( date > as.Date("2020-03-01") & date < as.Date("2020-07-01")) %>%
    merge(age_group) %>%
    group_by( country_hmd_code, age_supergroup ) %>%
    summarise( excess_death_percent =  (sum(death_observed) / sum(death_expected)) - 1 )

all_mortality_by_age_supergroup %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
  merge(countries) %>%
  ggplot( aes(x=country_name, y=excess_death_percent, fill=age_supergroup)) +
  geom_col( position = "dodge") +
  coord_flip() 




all_mortality_by_age_supergroup_dcast =
  all_mortality_by_age_supergroup %>%
    dcast( country_hmd_code ~ age_supergroup, value.var="excess_death_percent" ) %>%
    rename ( excess_death_infant = "00-04",
             excess_death_youth = "05-19",
             excess_death_active = "20-64", 
             excess_death_retired = "65-84",
             excess_death_elder = "85+")

all_mortality_by_age_supergroup %>%
  merge( countries ) %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 ) %>%
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
  filter( age_supergroup >= "65") %>%
  mutate( covid_death_percent = covid_death_count / death_expected ) %>%
  select(-country_hmd_code, -country_iso_code_2, -country_iso_code, -country_name, -covid_death_count, -death_expected ) %>%
  ggpairs ( ggplot2::aes(colour=age_supergroup, alpha = 0.5)  )
  



all_youth_mortality_by_sex =
  all_excess_death_2020 %>%
    filter( age_group >= "10" & age_group <= "15-19") %>%
    group_by( country_hmd_code, sex) %>%
    summarise( excess_death_percent = sum(excess_death) / sum(expected_death)) %>%
  arrange(excess_death_percent)

all_youth_mortality =
all_youth_mortality_by_sex %>% 
    group_by(country_hmd_code) %>% 
    summarise( excess_death_percent=mean(excess_death_percent)) %>%
    arrange(excess_death_percent)


all_youth_mortality_by_sex %>%
  ggplot(aes(x=country_hmd_code, y=excess_death_percent, fill = sex )) +
    geom_col( position = "dodge") +
   scale_x_discrete(limits=all_youth_mortality$country_hmd_code)



mobility_summary %>%
  merge( countries ) %>%
  merge(all_youth_mortality) %>%
  ggplot(aes(x=excess_death_percent, y=residential, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.005, nudge_y = 1.5, alpha = 0.5 ) +
  xlab("excess youth mortality") + ylab("residential") 



restriction_summary %>%
  merge( countries ) %>%
  merge(all_youth_mortality) %>%
  ggplot(aes(x=excess_death_percent, y=stringency, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.005, nudge_y = 1.5, alpha = 0.5 ) +
  xlab("excess youth mortality") + ylab("stringency") 



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


# Correlation of excess mortality during and excess mortality during autumn

all_death_expected_weekly %>%
  merge( all_country_summary ) %>%
  filter( max_week_2020 == 53 & year == 2020 &  date >= as.Date("2020-03-01") ) %>%
  mutate( period = if_else( date <= as.Date("2020-08-31"), "spring", "autumn") ) %>%
  group_by( period, country_hmd_code ) %>%
  summarise( excess_death_rate = sum(death_observed) / sum(death_expected)) %>%
  dcast( country_hmd_code ~ period, value.var="excess_death_rate" ) %>%
ggplot(aes(x=autumn, y=spring, label = country_hmd_code )) +
  geom_point() +
  geom_label( color = "blue", alpha = 0.5 ) 


all_death_expected_weekly %>%
  merge( all_country_summary ) %>%
  merge( countries ) %>%
  filter( max_week_2020 == 53 & year == 2020 &  date >= as.Date("2020-03-01") ) %>%
  group_by( country_iso_code_2, country_name ) %>%
  summarise( excess_death_rate = ( sum(death_observed) / sum(death_expected)) - 1) %>%
  merge( mobility_summary ) %>%
  mutate( parks = parks/ 100 ) %>%
  ggplot(aes(x=parks, y=excess_death_rate, label = country_name )) +
  geom_point() +
  geom_smooth( method = "lm", se = FALSE, alpha = 0.1) +
  geom_label( color = "blue", alpha = 0.5, nudge_x = 0.01, nudge_y = 0.01 )  +
  scale_x_continuous(labels =  scales::percent_format(accuracy = 1), name = "Increase in attendance of parks according to Google") +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1), name = "Excess mortality based on demographic dats") +
  labs( title = "Correlation between increase in park attendance and excess mortality rate")


save.image("./data/all_countries.Rdata")



