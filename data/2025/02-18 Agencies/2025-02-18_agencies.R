# tidytuesday #7 ----------------------------------------------------------

# 2025-02-18
# Agencies from the FBI Crime Data API

# prelims -----------------------------------------------------------------

library(tidyverse)

# load data
agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')
map_states <- map_data("county")

# wrangle -----------------------------------------------------------------

# issues:
#   - FBI data and the map data do not use county identifiers and/or 
#     standardized county names (bit of cleaning involved)
#   - Some agencies have identical lat/long for the entire county

## map data ----
map_states <- map_states |> 
  rename(county = subregion, state = region) |> 
  mutate(county = str_remove(county, " city$"),
         county = case_when(
           county %in% c("de kalb", "de soto", "de witt", "du page", "la moure") ~ str_remove(county, " "),
           .default = county
           ))

## agencies data ----
agencies <- agencies |> 
  mutate(across(all_of(c("county", "state")), tolower)) |> 
  filter(!ori %in% c("KY0710900")) |> 
  filter(!county %in% c("not specified")) |> 
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  mutate(county = gsub("^(\\S+?), .*$", "\\1", county)) |> 
  mutate(county = str_remove(county, " city$"),
         county = case_when(
    county %in% c("de kalb", "de soto") ~ str_remove(county, " "),
    county %in% c("o'brien", "prince george's", "queen anne's", "st mary's") ~ str_remove(county, "'"),
    .default = county
  ))


# engineer ----------------------------------------------------------------

# determine distances between agencies
mtx <- agencies |> 
  select(ori, latitude, longitude) |> 
  filter(!is.na(latitude)) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mtx_dist <- mtx |>
  sf::st_distance()

rownames(mtx_dist) <- mtx$ori

# for each row (~agency), sort and get lowest (mean) distances:
agencies_geo_inf <- apply(mtx_dist, 1, function(x) {
  # distances to next agency further than 100m (~yards):
  dist_next <- x[x > 100]
  dist_next <- sort(dist_next)[1]/1609.34
  # number of agencies within N miles:
  within_n <- sort(x)[2:length(x)]/1609.34
  within_10 <- sum(within_n < 11)
  within_5 <- sum(within_n < 6)
  within_2 <- sum(within_n < 3)
  # combine
  out <- c(dist_next, within_10, within_5, within_2)
  }) |> 
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column("ori") |> 
  rename(dist_next = 2, nearby_agencies_10 = 3, nearby_agencies_5 = 4, nearby_agencies_2 = 5)

# add geo info data to agencies:
agencies <- agencies |> 
  left_join(agencies_geo_inf, join_by(ori)) |> 
  # calculate county mean distances:
  group_by(state, county) |> 
  mutate(n_agencies = n(),
         mean_dist_next = mean(dist_next, na.rm = TRUE),
         median_nearby_10 = median(nearby_agencies_10, na.rm = TRUE),
         median_nearby_5 = median(nearby_agencies_5, na.rm = TRUE),
         median_nearby_2 = median(nearby_agencies_2, na.rm = TRUE)) |> 
  ungroup()

# final df to plot:
map_df <- map_states |> 
  left_join(
    agencies |> 
      select(
        county, state, n_agencies, mean_dist_next,
        median_nearby_10, median_nearby_5, median_nearby_2) |> 
      distinct(), join_by(county, state)) |> 
  as_tibble()

# description -------------------------------------------------------------

# how many counties have another agency within 5 miles?
agencies |> 
  filter(!is.na(nearby_agencies_5)) |> 
  mutate(nearby = if_else(nearby_agencies_5 > 0, "yes", "no")) |> 
  count(nearby) |> 
  mutate(prop = n/sum(n))

# 95% of agencies have another agency within 10 miles
# 86% of agencies have another agency within 5 miles

# visualize ---------------------------------------------------------------

## the distance of the most isolated agency in a county yields similar picture
## with lighter 
infotext1 <- "78% of agencies\nare within 2 miles\nof another agency"
infotext2 <- "88% within 5 miles\n95% within 10 miles"

title = "Population density through the lens of agency locations:\nHow far to the next agency?"
subtitle = "Average distance to the nearest agency not on the same premises (within ~100 yards)"
caption = "Data: FBI Crime Data API (#tidytuesday 2025-02-18) "

map_df |> 
  ggplot(aes(x = long, y = lat, group = group, fill = mean_dist_next)) + 
  geom_polygon() +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       fill = "Miles") +
  scale_fill_viridis_c(option = 'D', limits = c(0,80)) +
  coord_map() +
  theme_void(base_family = "Goldman Sans Condensed") +
  annotate("text", x = -124.6813, y = 25, label = infotext1, fontface = "bold", family = "Goldman Sans Condensed", size = 4, colour = "grey15", hjust = 0, vjust = 0, lineheight = 0.8) +
  annotate("text", x = -124.6813, y = 22, label = infotext2, family = "Goldman Sans Condensed", size = 3, colour = "grey15", hjust = 0, vjust = 0, lineheight = 0.8) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(.1, "cm"),
        plot.title = element_text(size = 14, face = "bold")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5),
         size = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("data/2025/02-18 Agencies/2025-02-18_agencies_distances.png", height = 5, width = 6, bg = "white")

