
# tidytuesday #7 ----------------------------------------------------------

# 2025-02-18

# Agencies from the FBI Crime Data API


# prelims -----------------------------------------------------------------

library(tidyverse)
library(usdata)

# load data
agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

# graphics & ci
# n/a

# ancillary data
data(county)

# wrangle -----------------------------------------------------------------

df_state <- map_data("county")

df_base <- agencies |>
  filter(
    longitude < -10,
    longitude > -125
  ) |>
  mutate(
    county = tolower(county),
    county_split = str_split(county, ", ")
  ) |>
  unnest(county_split) |>
  group_by(county = county_split) |>
  summarise(
    p = mean(is_nibrs),
    n = sum(is_nibrs)
  )

df_base <- df_state |>
  left_join(df_base, join_by(subregion == county))


ggplot(map.df, aes(x=long, y=lat, group=group, fill=obesity)) + 
  geom_polygon()+coord_map()+theme_void()

df_base |> 
  ggplot(aes(x=long, y=lat, group=group, fill = n)) +
  geom_polygon() + 
  coord_map() + 
  scale_fill_distiller(palette = "Greens", guide = "none", direction = 1) + theme_void()

ggplot() +
  geom_map(data = df_state, map = df_state,
           aes(long, lat, map_id = region)) +
  hrbrthemes::theme_ipsum_gs() +
  theme_void()


## sandbox
library(ggplot2)
# this creates an example formatted as your obesity.map - you have this already...
set.seed(1)    # for reproducible example
map.county2 <- map_data('county')
counties   <- unique(map.county[,5:6])
obesity_map <- data.frame(state_names=counties$region, 
                          county_names=counties$subregion, 
                          obesity= runif(nrow(counties), min=0, max=100))

# you start here...
library(data.table)   # use data table merge - it's *much* faster
map.county2 <- data.table(map_data('county'))
setkey(map.county,region,subregion)
obesity_map <- data.table(obesity_map)
setkey(obesity_map,state_names,county_names)
map.df      <- map.county[obesity_map]

ggplot(map.df, aes(x=long, y=lat, group=group, fill=obesity)) + 
  geom_polygon()+coord_map()+theme_void()


