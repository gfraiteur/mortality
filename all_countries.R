library( ggpubr)

selected_countries = c( "BEL", "FRATNP", "CHE", "ESP", "DEUTNP", "ITA", "CZE", "POL", "NLD", "DNK", "SWE", "FIN")

if ( exists("all_graphs")) rm( all_graphs )
if ( exists("all_excess_death_2020")) rm( all_excess_death_2020 )
if ( exists("all_years_with_more_mortality")) rm( all_years_with_more_mortality )


for ( current_country_hmd_code in selected_countries ) {
  source("generic.R")
}


selected_countries_info <- 
  selected_countries %>%
  merge( countries, by.x = c(1), by.y = "country_hmd_code" )  %>% 
  rename( country_hmd_code = x )
  
  

rownames(selected_countries_info) <- selected_countries_info$country_hmd_code

all_graphs_by_kind = list()

for ( country in names(all_graphs)) {
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
  geom_smooth() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "relative excess death rate (square root scale)",
                     limits = c(-1, 1.25), breaks = signed_square(seq(-1,2,0.1) ), trans = signed_sqrt_trans)  +
  scale_x_continuous(name = "age group", breaks = seq(0,100,5), minor_breaks = FALSE) +
  labs( title = "Relative excess death rate 2020 (all countries)") 

country_exceptionality = 
  all_years_with_more_mortality %>%
    group_by( country_hmd_code ) %>%
    summarise( excess_death_rate_exceptionality = mean(excess_death_rate_exceptionality), 
               death_rate_exceptionality = mean(death_rate_exceptionality)) %>%
    ungroup() %>%
    mutate( total_score = excess_death_rate_exceptionality + death_rate_exceptionality ) %>%
  arrange( total_score )


country_exceptionality %>%
  select(-total_score) %>%
  melt() %>%
    ggplot(aes(x=country, y=value, fill=variable)) +
    geom_col(position=position_stack())  +
    scale_x_discrete(limits=country_exceptionality$country)


###  Covid-19 government response stringency index

if ( !file.exists("./data/OxCGRT_latest.csv")) {
download.file("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", "./data/OxCGRT_latest.csv")
}

restrictions <- read_csv("data/OxCGRT_latest.csv", 
                              col_types = cols(CountryName = col_skip(), 
                         RegionName = col_skip(), RegionCode = col_skip(), 
                         Jurisdiction = col_skip(), Date = col_integer())) %>%
  merge( countries, by.x = c("CountryCode"), by.y = c("country_iso_code"), suffixes = c("","") ) %>%
  rename( country_iso_code = CountryCode ) %>%
  mutate( year = floor(Date/10000), month = floor( ( Date %% 10000 ) / 100  ), day = Date %% 100 ) %>% 
  mutate( date = make_date( year, month, day)) %>% select( -Date, -year, -month, -day) %>%
  complete( country_iso_code, date, fill= 0 )

restriction_summary =
  restrictions %>%
    filter( year(date) == 2020 ) %>%
    filter( country_hmd_code %in% selected_countries ) %>%
     group_by( country_hmd_code, country_iso_code ) %>%
     summarise( stringency = mean(StringencyIndex, na.rm = TRUE))

restriction_summary %>%
  merge(country_exceptionality) %>%
  ggplot(aes(x=total_score, y=stringency, label = country_iso_code )) +
  geom_point() +
  geom_label( color = "blue" )


restrictions %>%  
  filter( country_hmd_code %in% selected_countries ) %>%
ggplot(aes(x=date, y=StringencyIndex, color = country_iso_code )) +
  geom_line() 
