library(ggpubr)

selected_countries = c( "ESP", "BEL", "POL", "NLD", "ITA", "CZE", "FRATNP", "CHE", "DNK", "DEUTNP", "FIN", "SWE", "GRC", "HUN", "HRV", "EST", "SVN" )

if ( exists("all_graphs")) rm( all_graphs )
if ( exists("all_excess_death_2020")) rm( all_excess_death_2020 )
if ( exists("all_years_with_more_mortality")) rm( all_years_with_more_mortality )
if ( exists("all_life_expactancy")) rm( all_life_expactancy )


for ( current_country_hmd_code in selected_countries ) {
  source("generic.R")
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
  group_by( age_min,sex) %>%
  summarise(excess_death_rate = (sum(death_observed) / sum(expected_death)) -1  )  %>%
  ggplot(aes(x=age_min, y=excess_death_rate, color=sex, fill = sex)) +
  geom_point(aes()) +
  #  geom_line(aes()) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "relative excess death rate (square root scale)",
                     limits = c(-1, 1.25), breaks = signed_square(seq(-1,2,0.1) ), trans = signed_sqrt_trans)  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), minor_breaks = FALSE) +
  labs( title = "Relative excess death rate 2020 (all countries)") 

country_exceptionality = 
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
  mutate( excess_death_rate_regression = excess_death_rate_regression / max_excess_death_rate_regression,
          death_rate_regression =  death_rate_regression / max_death_rate_regression,
          death_increase =  death_increase / max_death_increase ) %>%
  mutate( total_score = excess_death_rate_regression + 
            death_rate_regression + 
            death_increase  ) %>%
  arrange( total_score ) 
  



country_exceptionality %>%
  select(country_hmd_code, death_rate_regression,  excess_death_rate_regression, death_increase ) %>%
  melt() %>%
    ggplot(aes(x=country_hmd_code, y=value, fill=variable)) +
    geom_col(position=position_stack())  +
    scale_x_discrete(limits=country_exceptionality$country_hmd_code)




all_life_expectancy %>%
  group_by( sex, age_group, country_hmd_code ) %>%
  summarise( life_expectancy = mean(life_expectancy)) %>%
  merge(all_excess_death_2020) %>%
  mutate( lost_years = excess_death * life_expectancy) %>%
  merge( age_group ) %>%
  ggplot( aes(x=age_min, y=lost_years, color=sex, fill = sex)) +
  geom_point() +
  geom_smooth() +
  this_theme( "Number of lost years of life expectancy")  +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), name = "years",
                     limits = c(-1200*population_axis_scale, 1200 * population_axis_scale),
                     sec.axis = sec_axis( population_axis_transform, name = population_axis_name))



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
  summarise( mobility_index = -mean( retail_and_recreation + transit_stations + workplaces ) / 3,
             retail_and_recreation = -mean(retail_and_recreation),
             transit_stations = -mean(transit_stations),
             workplaces = -mean(workplaces),
             parks = -mean(parks),
             residential = mean(residential))


# Mortality by stringency index
restriction_summary %>%
  merge(country_exceptionality) %>%
  ggplot(aes(x=total_score, y=stringency, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.5, nudge_y = 1 ) +
  xlab("excess mortality score") + ylab("stringency score") 

# Mortality by containment health index
restriction_summary %>%
  merge(country_exceptionality) %>%
  ggplot(aes(x=total_score, y=containment_health, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.5, nudge_y = 1 ) +
  xlab("excess mortality score") + ylab("containment index") 


mobility_summary %>%
  merge( countries ) %>%
  merge(country_exceptionality) %>%
  ggplot(aes(x=death_increase, y=retail_and_recreation, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue", nudge_x = -0.005, nudge_y = 1.5, alpha = 0.5 ) +
  geom_smooth( method = "lm", se = FALSE) +
  xlab("excess mortality score") + ylab("mobility decrease") 


# Correlation between death rate and mobility indexes
model_death_rate =
  aov( death_rate_regression ~ retail_and_recreation + transit_stations + workplaces + parks + residential, 
       mobility_summary %>%
          merge( countries ) %>%
          merge(country_exceptionality)
         ) 
  
model_excess_death_rate =
  aov( excess_death_rate_regression ~ retail_and_recreation + transit_stations + workplaces, 
       mobility_summary %>%
         merge( countries ) %>%
         merge(country_exceptionality)
  ) 



save.image("all_countries.Rdata")

