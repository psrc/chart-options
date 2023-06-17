library(rlang)
library(dplyr)
library(echarts4r)

# Build -------------------------------------------------------------

swap_column_chart <- function(t, x, y, facet, est, dec, title=NULL, subtitle=NULL){
  
  est <- dplyr::pull(t, {{ x }}) %>% psrcplot:::est_type_default()
  
  e_common(font_family = "Poppins")
  
  t |> 
    group_by(.data[[facet]]) %>% 
    arrange(desc(Race)) %>% 
    mutate(Race=stringr::str_wrap(Race,18))|> 
    e_charts_(x, timeline=TRUE) |> 
    #  e_grid(left = '15%') |>
    e_bar_(serie=y) |> 
    e_flip_coords() |>
    suppressWarnings(  # Tells user to open plot in browser
      e_x_axis_(formatter = e_axis_formatter(style=est, digits=dec))
    ) |>
    e_y_axis(axisLabel = list(interval = 0L)) |>
    e_legend(show = TRUE, bottom=0) |>
    e_title(title, subtitle) |>
    e_theme_custom("C:/Users/mjensen/projects/psrcplot/psrc_theme.json") |>
    e_tooltip() |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage") |>
    e_timeline_opts(autoPlay = FALSE,
                    axis_type = "category",
                    symbol = "emptyCircle",
                    top = "0.5%",
                    right = "20%",
                    left = "60%",
                    progress = NULL,
                    itemStyle = list(borderWidth = 3),
                    #label = list(fontWeight = "bold"),
                    checkpointStyle = list(color = "#000000"),
                    controlStyle = FALSE,
                    lineStyle = FALSE
    )
}

# Try ---------------------------------------------------------------
df <- psrcplot::mode_share_example_data %>%
  filter(Category=="Population by Race" & Year==2020) %>%
  filter(Race !="Total") %>% 
  mutate(Geography=factor(Geography, levels=c("Region","King","Kitsap","Pierce","Snohomish")),
         Race=factor(Race, levels=c("American Indian or Alaskan Native Alone","Asian","Black or African American",
                                    "Hispanic or Latino","Native Hawaiian and Other Pacific Islander","White",
                                    "Some Other Race","Two or More Races")))

swap_column_chart(df, x="Race", y="share", facet="Geography", est="percent", dec=1,
                  title="Population by Race 2020", 
                  subtitle="For counties in the Central Puget Sound Region")