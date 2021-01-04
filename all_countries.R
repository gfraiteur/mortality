library( ggpubr)

selected_countries = c( "BEL", "FRATNP", "CHE", "ESP", "DEUTNP", "ITA", "CZE", "POL", "NLD", "DNK", "SWE", "FIN")
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
                       legend = "top" 
                       )
  figure <- annotate_figure(figure, top = text_grob(trimws( graphs$title ), size = 18 ) )
  
  print(figure)
  
  }


for ( graphs in all_graphs_by_kind ) {
  print_graphs(graphs)
}
