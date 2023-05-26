library(tidyverse)
library(echarts4r)
library(psrcplot)

# Basic Inputs ------------------------------------------------------------
# This allows Poppins to be used in echarts4r
echarts4r::e_common(font_family = "Poppins")

# Data Prep ---------------------------------------------------------------
fatal_by_race <- read_csv("data/fatal_collision_data.csv", show_col_types = FALSE) %>% 
  filter(variable == "Race & Ethnicity" & metric == "1-year average for fatal collisions" & geography=="Region" & year==2021) %>%
  mutate(fatality_rate = round(fatality_rate,1)) %>%
  dplyr::arrange(fatality_rate) %>%
  dplyr::mutate(race= str_wrap(grouping, 15))

# Custom SVG ions from font awesome ---------------------------------------------------
# Uncomment the next two lines to find the svg values for a fontawesome icon to use in echarts4r
#library(fontawesome)
#my_logo <- fa("user-large")

fa_house <- "path://M575.8 255.5c0 18-15 32.1-32 32.1h-32l.7 160.2c0 2.7-.2 5.4-.5 8.1V472c0 22.1-17.9 40-40 40H456c-1.1 0-2.2 0-3.3-.1c-1.4 .1-2.8 .1-4.2 .1H416 392c-22.1 0-40-17.9-40-40V448 384c0-17.7-14.3-32-32-32H256c-17.7 0-32 14.3-32 32v64 24c0 22.1-17.9 40-40 40H160 128.1c-1.5 0-3-.1-4.5-.2c-1.2 .1-2.4 .2-3.6 .2H104c-22.1 0-40-17.9-40-40V360c0-.9 0-1.9 .1-2.8V287.6H32c-18 0-32-14-32-32.1c0-9 3-17 10-24L266.4 8c7-7 15-8 22-8s15 2 21 7L564.8 231.5c8 7 12 15 11 24z"
fa_person <- "path://M112 48a48 48 0 1 1 96 0 48 48 0 1 1 -96 0zm40 304V480c0 17.7-14.3 32-32 32s-32-14.3-32-32V256.9L59.4 304.5c-9.1 15.1-28.8 20-43.9 10.9s-20-28.8-10.9-43.9l58.3-97c17.4-28.9 48.6-46.6 82.3-46.6h29.7c33.7 0 64.9 17.7 82.3 46.6l58.3 97c9.1 15.1 4.2 34.8-10.9 43.9s-34.8 4.2-43.9-10.9L232 256.9V480c0 17.7-14.3 32-32 32s-32-14.3-32-32V352H152z"
fa_user <- "path://M256 288A144 144 0 1 0 256 0a144 144 0 1 0 0 288zm-94.7 32C72.2 320 0 392.2 0 481.3c0 17 13.8 30.7 30.7 30.7H481.3c17 0 30.7-13.8 30.7-30.7C512 392.2 439.8 320 350.7 320H161.3z"

# Functions ---------------------------------------------------------------
# A function to create a pictorial chart from echarts4r
create_pictogram <- function(df, x, vbl, icon_svg, title, title_link = NULL, hover, color) {
  
  picto_chart <- df %>% 
    group_by(year) %>%
    e_charts_(x=x) %>%
    e_color(color) %>%
    e_tooltip() %>%
    e_grid(left = '20%') %>%
    e_pictorial_(serie = vbl, 
                symbol = icon_svg,
                symbolRepeat = TRUE, z = -1,
                symbolSize = 30,
                legend = FALSE, 
                name = hover) %>%
    e_title(title, link=title_link) %>%
    e_flip_coords() %>%
    e_legend(show = FALSE) %>%
    e_x_axis(splitLine=list(show = FALSE)) %>% 
    e_y_axis(splitLine=list(show = FALSE),
             axisPointer = list(show = FALSE)) %>%
    e_toolbox_feature("dataZoom") %>%
    e_toolbox_feature(feature="reset") %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  return(picto_chart)
  
}

# Test Charts --------------------------------------------------------------
my_chart <- create_pictogram(df=fatal_by_race,
                             x="race",
                             vbl="fatality_rate",
                             icon_svg=fa_user,
                             title="Fatal Collision Rate by Race (2021)",
                             title_link="https://www.psrc.org/",
                             hover="Fatal Collisions per 100,000 people",
                             color= psrc_colors$pgnobgy_5[[1]])

my_chart_house <- create_pictogram(df=fatal_by_race,
                                   x="race",
                                   vbl="fatality_rate",
                                   icon_svg=fa_house,
                                   title="Fatal Collision Rate by Race and Hispanic/Latinx Origin (2021)",
                                   hover="Fatal Collisions per 100,000 people",
                                   color= psrc_colors$pgnobgy_5[[2]])





  
  

