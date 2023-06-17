library(tidyverse)
library(echarts4r)
library(psrcplot)

interactive_facet_column_chart <- function(t, x, y, facet, ncol=3, est=NULL,
                                           title=NULL, subtitle=NULL){
  
  nrow <- length(unique(t[[facet]])) / ncol %>% ceiling()
  est <- dplyr::pull(t, {{ x }}) %>% psrcplot:::est_type_default()
  
  t |> 
    group_by(.data[[facet]]) |> 
    e_charts_(x) |> 
    e_bar_(serie=y) |> 
    suppressWarnings(  # Tells user to open plot in browser
      e_y_axis(formatter = e_axis_formatter("percent", digits="1"))
    ) |>
    e_x_axis(axisLabel = list(interval = 0L)) |>
    e_facet(rows=nrow, 
            cols=ncol, 
            legend_pos = "bottom", 
            legend_space = 5,
            margin_trbl = c(t = 8, r = 2, b = 5, l = 2)) |>
    #e_labels(fontSize = 9,) |>
    e_legend(show = TRUE, bottom=0) |>
    e_title(title, subtitle,
            textAlign = list('left')#,
            #padding = list(c(5, 10, 5, 10))
            ) |>
    #e_theme_custom("C:/Users/mjensen/projects/psrcplot/psrc_theme.json") |>
    e_tooltip() |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage") |>
    e_grid(top = '15%')
}

df <- psrcplot::mode_share_example_data %>%
  filter(Category=="Population by Race" & Year==2020) %>%
  filter(Race !="Total") 

interactive_facet_column_chart(df, 
                               x = "Geography", 
                               y = "share", 
                               ncol = 4, 
                               facet = "Race", 
                               title = "Population by Race 2020", 
                               subtitle = "For counties in the Central Puget Sound Region")
