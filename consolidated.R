library( ggpubr)

selected_countries = c( "BEL", "FRATNP", "CHE", "ESP", "DEUTNP", "ITA")
rm ( all_graphs )



for ( country in selected_countries ) {
  source("generic.R")
}


selected_countries_info <- 
  selected_countries %>%
  merge( countries, by.x = c(1), by.y = "code" )  %>% 
  rename( code = x ) %>%
  select ( -iso_code)

rownames(selected_countries_info) <- selected_countries_info$code

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
                       legend = "bottom", 
                       labels = selected_countries_info[names(graphs_without_title),]$name )
  annotate_figure(figure, top = text_grob(trimws( graphs$title ), size = 18 ) )
  
  }


print_graphs( all_graphs_by_kind$population_structure)
print_graphs( all_graphs_by_kind$expected_death_per_week)
print_graphs( all_graphs_by_kind$cumulative_excess_death_per_year)
print_graphs( all_graphs_by_kind$covid_cumulative_death )
print_graphs( all_graphs_by_kind$excess_death_rate_2020 )
print_graphs( all_graphs_by_kind$year_with_comparable_death_rate  )
print_graphs( all_graphs_by_kind$year_with_comparable_excess_death_rate )
 
