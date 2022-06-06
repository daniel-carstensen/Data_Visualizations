## Data Visualization (GOVT16-QSS17) Spring 2022
## Final Exam
##
## Name: Daniel Carstensen
## Date: June 3 - June 5, 2022

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(gganimate)
library(lubridate)
library(rvest)
library(magick)
library(cowplot)
library(zoo)
library(gridExtra)
library(grid)
library(readxl)
library(waffle)


## 1.
geese <- read.csv('geese_problem/geese.csv')
lakes <- st_read('geese_problem/natural_usa/ne_10m_lakes/ne_10m_lakes.shp')
land <- st_read('geese_problem/natural_usa/ne_10m_land/ne_10m_land.shp')
rivers_lake <- st_read('geese_problem/natural_usa/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp')

lakes <- lakes %>%
  select(geometry)
land <- land %>%
  select(geometry)
rivers_lake <- rivers_lake %>%
  select(geometry)
geese <- geese %>%
  select(location.long, location.lat)

plot_1 <- geese %>%
  ggplot() +
  geom_sf(data = land, inherit.aes = FALSE, fill = '#c0c2ce', size = 0.2) +
  geom_sf(data = lakes, inherit.aes = FALSE, color = '#0986fd', fill = '#0986fd', size = 0.1) +
  geom_sf(data = rivers_lake, inherit.aes = FALSE, color = '#2160bf', fill = '#2160bf', size = 0.4) +
  geom_point(aes(x = location.long, y = location.lat), color = '#d22f28', size = 0.1) +
  coord_sf(xlim = c(-5, 90), ylim = c(35, 85)) +
  labs(title = 'White-Fronted Geese in Europe and Northern Asia\nObservations of Migrating Geese in Red',
       x = 'Longitude',
       y = 'Latitude') +
  theme_minimal() +
  theme(text = element_text(size = 10))

ggsave('plot_1.png', plot_1, width = 3600, height = 2550, units = 'px', bg = 'white')


## 2.
map <- ne_countries(scale = 'medium', returnclass = 'sf')
migration <- st_read('songbirds/points.shp')

glimpse(migration)

map <- map %>%
  filter(name != 'Antarctica') %>%
  select(geometry)
migration <- migration %>%
  filter(tag_ident %in% c('4210-002', '4210-004'),
         lat != 0) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  select(timestamp, tag_ident, geometry)

plot_2 <- migration %>%
  ggplot() +
  geom_sf(data = map, inherit.aes = FALSE, size = 0.2) +
  geom_sf(aes(color = tag_ident), size = 0.2) +
  coord_sf(xlim = c(-140, -40)) +
  scale_color_manual(values = c('#f7b556', '#32328f')) +
  labs(title = 'Two Blackpoll Warblers Migrating in the Americas',
       subtitle = '{frame_time}') +
  theme_minimal() +
  theme(text = element_text(size = 12),
        legend.position = 'none') +
  transition_time(time = timestamp) +
  shadow_wake(wake_length = 0.5) +
  exit_fade()

animate(plot_2, nframes = 150, height = 1100, width = 1200, units = 'px')
anim_save('plot_2.gif')


## 3.
rt_html <- read_html('https://editorial.rottentomatoes.com/guide/best-horror-movies-of-all-time/')

rt_year <- rt_html %>%
  html_nodes(css = '.start-year') %>%
  html_text()

rt_title <- rt_html %>%
  html_nodes(css = '.article_movie_title a') %>%
  html_text()

rt_critic <- rt_html %>%
  html_nodes(css = '.critics-consensus') %>%
  html_text()

rt_position <- rt_html %>%
  html_nodes(css = '.countdown-index') %>%
  html_text()

rt_poster <- rt_html %>%
  html_nodes(css = '.article_poster') %>%
  html_attr('src')

rt <- cbind(year_raw = rt_year, position = rt_position, title = rt_title, critic = rt_critic, poster = rt_poster) %>%
  as_tibble()

rt <- rt %>%
  mutate(year = parse_number(year_raw),
         title = paste(position, title , year_raw, sep = ' ')) %>%
  filter(year < 1970) %>%
  slice_tail(n = 5) %>%
  map_df(rev)

rt$critic <- c("Critics Consensus: Infamous for its shower scene, but immortal for its contribution to the horror genre.\nBecause Psycho was filmed with tact, grace, and art, Hitchcock didn't just create modern horror, he validated it.",
               "Critics Consensus: Arguably the first true horror film, The Cabinet of Dr. Caligari set a brilliantly high\nbar for the genre -- and remains terrifying nearly a century after it first stalked the screen.",
               "Critics Consensus: King Kong explores the soul of a monster -- making audiences\nscream and cry throughout the film -- in large part due to Kong's breakthrough special effects.",
               "Critics Consensus: One of the silent era's most influential masterpieces, Nosferatu's eerie, gothic feel --\nand a chilling performance from Max Schreck as the vampire -- set the template for the horror films that followed.",
               "Critics Consensus: Featuring Robert Mitchum's formidable performance as a child-hunting preacher,\nThe Night of the Hunter is a disturbing look at good and evil.")

posters <- image_read(as.vector(rt$poster)) %>%
  image_montage(tile = 1, geometry = '+0+25') %>%
  image_scale('100x500!')

text <- rt %>%
  ggplot() +
  geom_text(x = -0.05, y = 0.8, aes(label = critic), size = 3, hjust = 0) +
  facet_wrap(~ title, nrow = 5) +
  theme_void() +
  theme(plot.margin = margin(0.8, 0, 0, 5, "cm"),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, hjust = 0, vjust = 1.1, face = 'bold'))

plot_3 <- ggdraw(text) +
  draw_image(posters, x = -0.4)

ggsave('plot_3.png', plot_3, width = 2400, height = 1600, units = 'px', bg = 'white')


## 4.
billboard <- read.csv('billboard_dat/hot_stuff.csv')
spotify <- read.csv('billboard_dat/hot100_audio_features.csv')

glimpse(billboard)
glimpse(spotify)

billboard_select <- billboard %>%
  select(WeekID, SongID, Weeks.on.Chart) %>%
  mutate(WeekID = as.Date(WeekID, format = '%m/%d/%Y'))
spotify_select <- spotify %>%
  filter(!is.na(danceability)) %>%
  select(SongID, danceability)

anti_join(billboard_select, spotify_select, by = 'SongID')
anti_join(spotify_select, billboard_select, by = 'SongID')

billboard_full <- inner_join(billboard_select, spotify_select, by = 'SongID')

glimpse(billboard_full)

billboard_full$danceability_level <- cut(billboard_full$danceability,
                                         labels = c('Level 1', 'Level 2', 'Level 3', 'Level 4', 'Level 5'),
                                         5)

plot_4 <- billboard_full %>%
  mutate(month = floor_date(WeekID, unit = 'month')) %>%
  group_by(month, danceability_level) %>%
  summarize(avg_week = mean(Weeks.on.Chart)) %>%
  mutate(year = floor_date(month, unit = 'year')) %>%
  ggplot(aes(x = year, y = avg_week, color = danceability_level),) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_smooth(span = 2) +
  scale_color_manual(values = c('#5734ff', '#1fa400', '#c9c233', '#ee9000', '#d64420'),
                     guide = guide_legend(reverse = TRUE)) +
  labs(title = 'Slow Songs No More! Billboard Top 100: 1958-2019',
       x = 'Year',
       y = 'Average Weeks on Billboard 100',
       color = 'Danceability') +
  theme_minimal() +
  theme(text = element_text(size = 10))

ggsave('plot_4.png', plot_4, width = 3150, height = 2190, units = 'px', bg = 'white')


## 5.
hanover <- read.csv('hanover_weather/hanover_noaa_data.csv')

glimpse(hanover)

hanover_temp <- hanover %>%
  filter(str_sub(DATE, -2) != '29') %>%
  select(DATE, TMAX, TMIN, TOBS) %>%
  mutate(tmax = na.approx(TMAX, na.rm = FALSE, rule = 2),
         tmin = na.approx(TMIN, na.rm = FALSE, rule = 2),
         month_day = format(as.Date(DATE,format='%Y-%m-%d'), format = '%m-%d')) %>%
  group_by(month_day) %>%
  summarize(avg_obs = mean(TOBS, na.rm = TRUE), tmax = max(tmax), tmin = min(tmin)) %>%
  mutate(year = 1,
         date = as.Date(paste(year, month_day, sep = '-')))

date_breaks <- c(as.Date('0001-01-01'), as.Date('0001-02-01'), as.Date('0001-03-01'), as.Date('0001-04-01'), as.Date('0001-05-01'), as.Date('0001-06-01'),
                 as.Date('0001-07-01'), as.Date('0001-08-01'), as.Date('0001-09-01'), as.Date('0001-10-01'), as.Date('0001-11-01'), as.Date('0001-12-01'))
temp_plot <- hanover_temp %>%
  ggplot() +
  geom_rect(aes(xmin = date, ymin = tmin, xmax = lead(date), ymax = tmax), color = '#740b01', fill = '#740b01') +
  geom_line(aes(x = date, y = avg_obs), color = '#d4a731', size = 1) +
  geom_rect(aes(xmin = as.Date('0001-07-15'), xmax = as.Date('0001-09-20'), ymin = -18, ymax = 18), color = '#dcdcdc', alpha = 0) +
  geom_rect(aes(xmin = as.Date('0001-07-19'), xmax = as.Date('0001-07-20'), ymin = -16, ymax = 16), color = '#740b01', fill = '#740b01') +
  geom_rect(aes(xmin = as.Date('0001-07-18'), xmax = as.Date('0001-07-21'), ymin = -4, ymax = 4), color = '#d5a932', fill = '#d5a932') +
  geom_rect(aes(xmin = as.Date('0001-07-22'), xmax = as.Date('0001-07-29'), ymin = 15.5, ymax = 16), color = '#dcdcdc', fill = '#dcdcdc') +
  geom_rect(aes(xmin = as.Date('0001-07-22'), xmax = as.Date('0001-07-29'), ymin = -16, ymax = -15.5), color = '#dcdcdc', fill = '#dcdcdc') +
  geom_rect(aes(xmin = as.Date('0001-07-23'), xmax = as.Date('0001-07-30'), ymin = -0.5, ymax = 0), color = '#dcdcdc', fill = '#dcdcdc') +
  geom_text(aes(x = as.Date('0001-08-02'), y = 15.75), label = 'Period Day High', vjust = 0.5, hjust = 0, size = 1.8, color = '#4f4f4f') +
  geom_text(aes(x = as.Date('0001-08-02'), y = -15.75), label = 'Period Day Low', vjust = 0.5, hjust = 0, size = 1.8, color = '#4f4f4f') +
  geom_text(aes(x = as.Date('0001-08-03'), y = 0), label = 'Average Daily Observed', vjust = 0.5, hjust = 0, size = 1.8, color = '#4f4f4f') +
  scale_x_date(breaks = date_breaks, date_labels = '%B', limits = c(as.Date('0001-01-01'), as.Date('0001-12-31'))) +
  scale_y_continuous(breaks = c(-40, -20, 0, 20, 40, 60, 80, 100, 120), limits = c(-40, 120)) +
  labs(title = 'Daily Temperatures Since 2000 for Hanover, New Hampshire',
       y = 'Temperature in Fahrenheit') +
  theme_minimal() +
  theme(text = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = -0.4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
    
hanover_prcp <- hanover %>%
  filter(str_sub(DATE, -2) != '29') %>%
  select(DATE, PRCP) %>%
  mutate(month_day = format(as.Date(DATE,format='%Y-%m-%d'), format = '%m-%d')) %>%
  group_by(month_day) %>%
  summarize(avg_prcp = mean(PRCP, na.rm = TRUE)) %>%
  mutate(month = str_sub(month_day, end = 2)) %>%
  group_by(month) %>%
  mutate(cum_prcp = cumsum(avg_prcp),
         year = 1,
         date = as.Date(paste(year, month_day, sep = '-')))

prcp_plot <- hanover_prcp %>%
  ggplot(aes(x = date, y = cum_prcp)) +
  geom_line() +
  scale_x_date(breaks = date_breaks, date_labels = '%B', limits = c(as.Date('0001-01-01'), as.Date('0001-12-31'))) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  labs(title = 'Average Cumulative Precipitation by Month for Hanover, New Hampshire',
       y = 'Inches') +
  theme_minimal() +
  theme(text = element_text(size = 6),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = -0.4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

plot_5 <- grid.arrange(temp_plot, prcp_plot, heights = c(5, 1))

ggsave('plot_5.png', plot_5, width = 2400, height = 1600, units = 'px', bg = 'white')


## 6.
crime <- read.csv('us_crimes_dat/us_crimes_histdat_f21.csv')
 
glimpse(crime)

crime_dc <- crime %>%
  filter(city == 'Washington, DC') %>%
  mutate(arrests_pp = arrests * 1000 / population)

## https://www.biggestuscities.com/city/lowell-massachusetts
crime[1179, 4] <- 112759

plot_6 <- crime %>%
  filter(city != 'Washington, DC') %>%
  group_by(city) %>%
  mutate(arrests_pp = arrests * 1000 / population) %>%
  ggplot(aes(x = year, y = arrests_pp, group = city)) +
  geom_line(color = '#e5e5e5', size = 0.4, alpha = 0.6) +
  geom_line(data = crime_dc, color = '#ff908f', size = 0.4, alpha = 0.7) +
  geom_text(aes(x = 1872, y = 310), label = 'Washington, D.C.', hjust = 0, size = 2, color = '#414141') +
  ylim(0, 450) +
  labs(title = 'U.S. Arrests, 1860-1920',
       subtitle = '23 Major Historical Cities',
       x = 'Year',
       y = 'Total Arrests per 1000 People') +
  theme_minimal() +
  theme(text = element_text(size = 7))

ggsave('plot_6.png', plot_6, width = 2600, height = 1600, units = 'px', bg = 'white')


## 7.
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

wiid <- read_xlsx('WIID_31MAY2021_0/WIID_31MAY2021.xlsx')

plot_7 <- wiid %>%
  select(country, q1:q5) %>%
  filter(country %in% c('United States', 'Canada')) %>%
  pivot_longer(names_to = 'quintiles',
               values_to = 'values',
               q1:q5) %>%
  filter(values > 0) %>%
  mutate(quintiles = factor(quintiles, 
                          levels = c('q1', 'q2', 'q3', 'q4', 'q5'),
                          labels = c('1st Quintile', '2nd Quintile', '3rd Quintile',
                                     '4th Qunitile', '5th Quintile'))) %>%
  group_by(country, quintiles) %>%
  summarize(total_values = sum(values)) %>%
  group_by(country) %>%
  mutate(percentage = total_values / sum(total_values) * 100) %>%
  ggplot(aes(values = round_preserve_sum(percentage), fill = quintiles)) +
  geom_waffle(color = 'white', size = 1) +
  scale_fill_manual(values = c('#2170be', '#52b283', '#5eab3e', '#9bd337', '#ded72e')) +
  labs(title = 'Income Inequality in 2010: U.S. v. Canada',
       fill = 'quintfact') +
  facet_wrap(~ country) +
  theme_void() +
  theme(text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position =  'bottom',
        legend.key.size = unit(2, 'mm')) 

ggsave('plot_7.png', plot_7, width = 2500, height = 1250, units = 'px', bg = 'white')


## 8.
asia <- c('Myanmar', 'Singapore', 'China', 'Bangladesh', 'Israel', 'Turkey', 'Sri Lanka',
          'Georgia', 'Thailand', 'Vietnam', 'Maldives', 'West Bank and Gaza', 'Indonesia',
          'Cambodia', 'India', 'Armenia', 'Mongolia', 'Cyprus', 'Japan', 'Korea, Repulic of',
          'Taiwan', 'Kyrgyztan', 'Kazakhstan')

americas <- c('Guatemala', 'Colombia', 'Honduras', 'Paraguay', 'Panama', 'Dominican Republic',
              'Costa Rica', 'Ecuador', 'Mexico', 'Peru', 'Barbados', 'Argentina', 'El Salvador',
              'Uruguay', 'United States', 'Jamaica', 'Venezuela', 'Canada', 'Greenland')

gini_asia <- wiid %>%
  filter(country %in% asia,
         year == 2010) %>%
  select(gini)

gini_americas <- wiid %>%
  filter(country %in% americas,
         year == 2010) %>%
  select(gini)

plot_asia <- wiid %>%
  filter(country %in% asia,
         year == 2010) %>%
  group_by(country) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(country, mean_gini), y = (mean_gini - mean(gini_asia$gini, na.rm = TRUE)))) +
  geom_bar(stat = 'identity', fill = '#6778bf') +
  scale_y_continuous(breaks = c(-5, 0, 5, 10)) + 
  coord_flip() +
  labs(title = 'Asian Gini Index Scores') +
  theme_minimal() +
  theme(text = element_text(size = 6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_line())

plot_americas <- wiid %>%
  filter(country %in% americas,
         year == 2010) %>%
  group_by(country) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(country, mean_gini), y = (mean_gini - mean(gini_americas$gini, na.rm = TRUE)))) +
  geom_bar(stat = 'identity', fill = '#246d0d') +
  scale_y_continuous(breaks = c(-10, -5, 0, 5)) + 
  coord_flip() +
  labs(title = 'Americas Gini Index Scores',
       caption = 'Source: World Income Inequality Database') +
  theme_minimal() +
  theme(text = element_text(size = 6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_line())

plot_8 <- grid.arrange(plot_asia, plot_americas, ncol = 2, top = textGrob('Centered Gini Index Scores by Region in 2010', gp = gpar(fontsize = 8)))

ggsave('plot_8.png', plot_8, width = 2670, height = 1370, units = 'px', bg = 'white')  


## 9.
plankton <- read.csv('aus_spatial_dat/copepods_standardised.csv')
aus_map <- st_read('aus_spatial_dat/Aussie.shp')

glimpse(plankton)
glimpse(aus_map)

plot_9 <- aus_map %>%
  ggplot() +
  geom_density_2d_filled(data = plankton, aes(x = longitude, y = latitude)) +
  scale_fill_manual(values = c('transparent', '#cbd6f1', '#bfd0f1', '#acc5f0', '#9dbcef', '#8db2ef',
                               '#79a7ef', '#659dee', '#5897ee', '#4b8fec', '#4088ea', '#3582e9')) +
  geom_sf(fill = '#075800', color = 'grey80', size = 0.15) +
  geom_sf_text(aes(label = adm), color = 'grey80', size = 2) +
  scale_x_continuous(breaks = c(90, 100, 110, 120, 130, 140, 150, 160), limits = c(88, 165)) +
  labs(title = 'Australian Copepods',
       subtitle = 'Plankton and Crustacean Concentrations',
       x = 'Longitude',
       y = 'Latitude') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = '#dadef2',
                                        colour = '#dadef2',
                                        size = 0.5, linetype = 'solid'))

ggsave('plot_9.png', plot_9, width = 3000, height = 2700, units = 'px', bg = 'white') 
