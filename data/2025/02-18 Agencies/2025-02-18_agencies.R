
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

## map data ----
map_states <- map_data("county") |> 
  rename(county = subregion,
         state = region)

## agencies data ----
agencies <- agencies |> 
  # remove alaska/hawaii
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  # mutate county name to match values in map_states:
  mutate(across(all_of(c("county", "state")), tolower)) |> 
  mutate(county = gsub("^(\\S+?), .*$", "\\1", county)) |>
  # add number of agencies per county:
  group_by(county, state) |> 
  mutate(n_agencies = n()) |> 
  ungroup()

# determine mean distances between agencies (~5mins)
mtx <- agencies |> 
  select(ori, latitude, longitude) |> 
  filter(!is.na(latitude)) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mtx_dist <- mtx |> 
  sf::st_distance()
rownames(mtx_dist) <- mtx$ori

# for each row, fetch lowest N values, determine mean
agencies_geo_inf <- apply(mtx_dist, 1, function(x) {
  # mean distance to nearest N agencies:
  means <- sort(x)[1:6]
  means <- mean(means)/1609.34
  # number of agencies within N miles:
  within_1 <- sort(x)[2:length(x)]/1609.34
  within_1 <- sum(within_1 < 10)
  # combine
  out <- c(means, within_1)
  }) |> 
  t() |> as.data.frame() |> 
  tibble::rownames_to_column("ori") |> 
  rename(mean_dist = 2, nearby_agencies = 3)

# add to agencies meta:
agencies <- agencies |> 
  left_join(agencies_geo_inf, join_by(ori)) |> 
  # mean distance for KY0710900 needs resetting b/c erroneous lat/long:
  mutate(mean_dist = case_when(ori == "KY0710900" ~ NA,
                               .default = mean_dist)) |> 
  # calculate county mean distances:
  group_by(state, county) |> 
  mutate(mean_dist_county = mean(mean_dist, na.rm = TRUE),
         median_nearby_county = median(nearby_agencies, na.rm = TRUE)) |> 
  ungroup()

rm(mtx, mtx_dist, agencies_geo_inf)

## county meta ----
df_county <- county |> 
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  # clean county name:
  mutate(county = str_remove(name, "\\."), .after = name) |> 
  mutate(county = str_remove(county, " *(Borough|County|Census Area|Municipality|Parish)")) |> 
  # lower county because that's how it is in agencies:
  mutate(across(all_of(c("county", "state")), tolower)) |> 
  # select cols:
  select(county, state, pop2017, poverty, homeownership, unemployment_rate)

## combine ----

df_data <- df_county |> 
  left_join(agencies |> select(county, state, n_agencies, mean_dist_county) |> distinct(), join_by(county, state)) |> 
  mutate(agencies_per_capita = n_agencies / pop2017 * 10000)

# final df to plot:
map_states <- map_states |> 
  left_join(df_data) |> 
  as_tibble()


# description -------------------------------------------------------------

# how many counties have another agency within 5 miles?
agencies |> 
  mutate(within_5 = if_else())

# visualize ---------------------------------------------------------------

map_states |> 
  ggplot(aes(log(mean_dist_county))) +
  geom_histogram()

ggplot(map_states, aes(x=long, y=lat, group=group, fill=mean_dist_county)) + 
  geom_polygon() +
  labs(title = "Mean distance between agencies",
       subtitle = "stuff",
       fill = "Miles") +
  scale_fill_viridis_c(option = 'D') +
  coord_map() +
  theme_void(base_family = "Goldman Sans Condensed") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.1, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 1),
         size = guide_legend(title.position = "top", title.hjust = 0.5))


