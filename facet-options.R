library(tidyverse)
library(echarts4r)
library(psrcplot)

interactive_facet_column_chart <- function(t, x, y, facet, ncol=3, est=NULL,
                                           title=NULL, subtitle=NULL){
  
  nrow <- length(unique(t[[facet]])) / ncol %>% ceiling()
  est <- dplyr::pull(t, {{ y }}) %>% psrcplot:::est_type_default()
  
  t |> 
    group_by(.data[[facet]]) |> 
    e_charts_(x) |> 
    e_bar_(serie=y) |> 
      e_y_axis(formatter = e_axis_formatter("percent", digits="1")) |>
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
  filter(Category=="Population by Race") %>%
  filter(Race !="Total") %>% 
  mutate(Geography=factor(Geography, levels=c("Region","King","Kitsap","Pierce","Snohomish")),
         Race=factor(Race, levels=c("American Indian or Alaskan Native Alone","Asian","Black or African American",
                                    "Hispanic or Latino","Native Hawaiian and Other Pacific Islander","White",
                                    "Some Other Race","Two or More Races")))

interactive_facet_column_chart(df, 
                               x = "Year", 
                               y = "share", 
                               ncol = 4, 
                               facet = "Race", 
                               title = "Population by Race 2020", 
                               subtitle = "For counties in the Central Puget Sound Region") |> 
  
  swap_column_chart(x="Race", y="share", facet="Geography", est="percent", dec=1,
                    title="Population by Race 2020", 
                    subtitle="For counties in the Central Puget Sound Region")
