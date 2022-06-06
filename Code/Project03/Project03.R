## Data Visualization (GOVT16-QSS17) Spring 2022
## Project 03: Animated Choropleth of the US by States in comparison to Europe by Vaccination Rate in 2021
##
## Name: Daniel Carstensen
## Date: May 30th


library(tidyverse)
library(gganimate)
library(USAboundaries)
library(USAboundariesData)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)
library(transformr)
library(sf)
library(tmap)
library(magick)
library(gapminder)


## Data Wrangling Europe
vaccination_world_base <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
countries <- ne_countries(scale = "medium", returnclass = "sf")

glimpse(vaccination_world_base)
glimpse(countries)

vaccination_europe <- vaccination_world_base %>%
  mutate(date = as.Date(date)) %>%
  rename(iso_a3 = iso_code) %>%
  group_by(iso_a3) %>%
  mutate(vax = na.approx(people_fully_vaccinated_per_hundred, na.rm = FALSE, rule = 2)) %>%
  filter(date >= "2021-01-01" & date < "2022-01-01",
         continent == "Europe") %>%
  select(iso_a3, date, vax) %>%
  ungroup()
  
glimpse(vaccination_europe)

france <- countries %>%
  filter(name == "France") %>%
  select(iso_a3, name, geometry)
france$iso_a3 <- "FRA"
norway <- countries %>%
  filter(name == "Norway") %>%
  select(iso_a3, name, geometry)
norway$iso_a3 <- "NOR"
kosovo <- countries %>%
  filter(name == "Kosovo") %>%
  select(iso_a3, name, geometry)
kosovo$iso_a3 <- "OWID_KOS"

countries <- countries %>%
  filter(iso_a3 != "ATA",
         iso_a3 != "RUS",
         region_un == "Europe") %>%
  select(iso_a3, name, geometry)

countries <- rbind(countries, france, norway, kosovo)

anti_join(vaccination_europe, countries, by = "iso_a3") %>%
  distinct(iso_a3)
anti_join(countries, vaccination_europe, by = "iso_a3") %>%
  distinct(iso_a3)

vaccination_europe_map <- merge(countries, vaccination_europe)


## Data Wrangling USA
election <- read.csv("election_president.csv")
vaccination_states <- read.csv("vaccination_by_state.csv")
states <- us_states()

glimpse(election)
glimpse(vaccination_states)
glimpse(states)

election <- election %>%
  filter(year == 2020,
         party_simplified == "DEMOCRAT" | party_simplified == "REPUBLICAN") %>%
  mutate(state = state_po,
         winner = str_to_title(party_simplified)) %>%
  select(state, candidatevotes, winner) %>%
  group_by(state) %>%
  slice_max(candidatevotes, n = 1) %>%
  select(state, winner) %>%
  ungroup()

vaccination_states <- vaccination_states %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(date >= "2021-01-01" & date < "2022-01-01") %>%
  select(Location, date, Series_Complete_Pop_Pct) %>%
  rename(state = Location, vax = Series_Complete_Pop_Pct)

states <- states %>%
  mutate(state = state_abbr) %>%
  select(state, name, geometry)

anti_join(vaccination_states, states, by = "state") %>%
  distinct(state)
anti_join(states, vaccination_states, by = "state") %>%
  distinct(state)

vaccination_states_map <- merge(states, vaccination_states, full = FALSE)


## Animated Choropleth of Europe and the US
glimpse(vaccination_states_map)
glimpse(vaccination_europe_map)

vaccination_states_map_prep <- vaccination_states_map %>%
  rename(abbr = state) %>%
  mutate(location = "USA")
vaccination_europe_map_prep <- vaccination_europe_map %>%
  rename(abbr = iso_a3) %>%
  mutate(location = "Europe")

bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 72), crs = st_crs(vaccination_europe_map_prep))
vaccination_europe_map_prep_cropped <- st_crop(vaccination_europe_map_prep, bbox_europe)

full_map <- rbind(vaccination_states_map_prep, vaccination_europe_map_prep_cropped)
full_map <- full_map %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico"))

pal <- c("#C3583E", "#dba687", "#efdbcb", "#b3d0c6", "#6fa094", "#246153")

animation <- tm_shape(full_map) + 
  tm_fill(col = "vax", style = "fixed", title = "Vaccination Pct",
          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), 
          legend.hist = TRUE,
          palette = pal) +
  tm_borders(col = "grey30", lwd = 0.25, lty = "solid", alpha = NA) +
  tm_layout(panel.labels = c("Vaccination Rate in Europe", "Vaccination Rate in the US"),
            panel.label.color = "grey5",
            panel.label.bg.color = "#d9d9d9",
            panel.label.size = 0.8,
            panel.label.height = 1.75,
            legend.title.color = "grey5",
            legend.text.color = "grey5",
            legend.position = c("left", "bottom"),
            legend.hist.width = 0.25,
            legend.hist.size = 0.25,
            bg.color = "#FDFDFD",
            frame = "grey5",
            frame.lwd = 0.35,
            fontfamily = "Helvetica Neue") +
  tm_facets(by = "location", along = "date", free.scales = TRUE)

tmap_animation(animation, "vaccine_animation.gif", loop = TRUE, delay = 20, dpi = 300, height = 1000)


## Animated Top 10 European Countries and US Party Affiliation
anti_join(vaccination_states, election, by = "state") %>%
  distinct(state)
anti_join(election, vaccination_states, by = "state") %>%
  distinct(state)

vaccination_states_election <- inner_join(vaccination_states, election, by = "state")
vaccination_states_election <- vaccination_states_election %>%
  rename(country = winner) %>%
  group_by(country, date) %>%
  summarize(vax = mean(vax, na.rm = TRUE)) %>%
  ungroup()

vaccination_full <- as.data.frame(vaccination_europe_map)%>%
  select(name, date, vax) %>%
  rename(country = name) %>%
  filter(!country %in% c("Monaco", "Guernsey", "Isle of Man", "Jersey", "Malta", "San Marino",
                         "Faeroe Is.", "Andorra", "Liechtenstein")) %>%
  group_by(date) %>%
  arrange(desc(vax)) %>%
  slice(1:10)

vaccination_full <- rbind(vaccination_full, vaccination_states_election)

vaccination_full$Title <- "Top 10 European Countries and the US by Party Affiliation by Vaccination Rate"

vaccination_history <- vaccination_full %>%
  mutate(rank = rank(vax, ties.method = "first")) %>%
  ggplot(aes(x = rank, y = vax, fill = as.factor(country))) +
  geom_bar(stat = "identity", position = "identity", width = 0.8) +
  geom_text(aes(label = country), hjust = -0.2, size = 1.75, color = "grey5") +
  facet_wrap(~ Title) +
  labs(y = "Vaccination Rate (in %)") +
  scale_fill_manual(values = c(country_colors, "Republican" = "#DE0100", "Democrat" = "#0015BC",
                               "Lithuania" = "#97DA8D", "Macedonia" = "#9EE393", "Bosnia and Herz." = "#A5EA9A",
                               "Moldova" = "#8ED583", "Luxembourg" = "#8AD87E", "Estonia" = "#9CCC94",
                               "Slovakia" = "#94D78A", "Czechia" = "#76AE6D", "Belarus" = "#89C97F", "Kosovo" = "#ADD894")) +
  ylim(0, 100) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 10, family = "Helvetica Neue"),
        title = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey5", fill = NA, size = 0.15),
        panel.background = element_rect(fill = "#FDFDFD"),
        strip.background = element_rect(fill = "#d9d9d9", size = 0.15)) +
  transition_manual(frames = date) +
  ease_aes() +
  enter_fade() +
  exit_fade()

bar_gif <- animate(vaccination_history, length(unique(vaccination_full$date)), res = 300,
                   height = 500, width = 1820, renderer = magick_renderer())


## Combine gifs
map_gif <- image_read("vaccine_animation.gif")

final_gif <- image_append(c(map_gif[1], bar_gif[1]), stack = TRUE)

for(i in 2:365) {
  combined <- image_append(c(map_gif[i], bar_gif[i]), stack = TRUE)
  final_gif <- c(final_gif, combined)
}

image_write(final_gif, "project03.gif", format = "gif")
    