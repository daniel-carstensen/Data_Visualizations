## Data Visualization (GOVT16-QSS17) Spring 2022
## Project 02
##
## Name: Daniel Carstensen
## Date: May 16th

library(tidyverse)
library(gganimate)
library(magick)
library(zoo)
library(lubridate)
library(cdlTools)
library(transformr)

jan <- read.csv("jan.csv")
feb <- read.csv("feb.csv")
mar <- read.csv("mar.csv")
apr <- read.csv("apr.csv")
may <- read.csv("may.csv")
jun <- read.csv("jun.csv")
jul <- read.csv("jul.csv")
aug <- read.csv("aug.csv")
sep <- read.csv("sep.csv")
oct <- read.csv("oct.csv")
nov <- read.csv("nov.csv")
dec <- read.csv("dec.csv")

glimpse(jan)
glimpse(feb)
glimpse(mar)
glimpse(apr)
glimpse(may)
glimpse(jun)
glimpse(jul)
glimpse(aug)
glimpse(sep)
glimpse(oct)
glimpse(nov)
glimpse(dec)

jan <- jan %>%
  mutate(month = as.Date(as.yearmon("2021-01"))) %>%
  mutate(exaggerated = q2,
         worried = q3,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

feb <- feb %>%
  mutate(month = as.Date(as.yearmon("2021-02"))) %>%
  mutate(exaggerated = q9,
         worried = q10,
         vax = vacc1) %>%
  select(exaggerated, worried,  vax, party, month)

mar <- mar %>%
  mutate(month = as.Date(as.yearmon("2021-03"))) %>%
  mutate(exaggerated = q8,
         worried = q5,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

apr <- apr %>%
  mutate(month = as.Date(as.yearmon("2021-04"))) %>%
  mutate(exaggerated = q3,
         worried = q4,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

may <- may %>%
  mutate(month = as.Date(as.yearmon("2021-05"))) %>%
  mutate(exaggerated = q23,
         worried = q21a,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

jun <- jun %>%
  mutate(month = as.Date(as.yearmon("2021-06"))) %>%
  mutate(exaggerated = q1,
         worried = q4a,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

jul <- jul %>%
  mutate(month = as.Date(as.yearmon("2021-07"))) %>%
  mutate(exaggerated = q1,
         worried = q2,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

aug <- aug %>%
  mutate(month = as.Date(as.yearmon("2021-08"))) %>%
  mutate(exaggerated = q1,
         worried = q2,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

sep <- sep %>%
  mutate(month = as.Date(as.yearmon("2021-09"))) %>%
  mutate(exaggerated = q1,
         worried = q3a,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

oct <- oct %>%
  mutate(month = as.Date(as.yearmon("2021-10"))) %>%
  mutate(exaggerated = q1,
         worried = q10a,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

nov <- nov %>%
  mutate(month = as.Date(as.yearmon("2021-11"))) %>%
  mutate(exaggerated = q2,
         worried = q6a,
         vax = vacc1) %>%
  select(exaggerated, worried, vax, party, month)

dec <- dec %>%
  mutate(month = as.Date(as.yearmon("2021-12"))) %>%
  mutate(exaggerated = NA,
         worried = vaxq1,
         vax = vaxq2) %>%
select(exaggerated, worried, vax, party, month)

twentyone <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

twentyone %>%
  count(exaggerated)
twentyone %>%
  count(worried)
twentyone %>%
  count(vax)
twentyone %>%
  count(party)

twentyone <- twentyone %>%
  mutate(
    exaggerated = case_when(
    str_detect(exaggerated, "Generally exaggerated") ~ "Exaggerated",
    TRUE ~ NA_character_),
    worried = case_when(
      str_detect(worried, "Not at all worried") ~ "Low concern",
      str_detect(worried, "Not too worried") ~ "Low concern",
      TRUE ~ NA_character_),
    vax = case_when(
      str_detect(vax, regex("Yes.|Yes", dotall = TRUE)) ~ "Vaccinated",
      str_detect(vax, regex("Got.", dotall = TRUE)) ~ "Vaccinated",
      str_detect(vax, regex("No.|No", dotall = TRUE)) ~ "Unvaccinated",
      TRUE ~ NA_character_),
    party = case_when(
      str_detect(party, "Democrat") ~ "Democrat",
      str_detect(party, "Republican") ~ "Republican",
      TRUE ~ NA_character_
    ))

twentyone_exaggerated <- twentyone %>%
  group_by(month, party) %>%
  count(exaggerated) %>%
  mutate(pct_exaggerated = n / sum(n) * 100) %>%
  na.omit() %>%
  select(month, party, pct_exaggerated) %>%
  ungroup()

twentyone_unworried <- twentyone %>%
  group_by(month, party) %>%
  count(worried) %>%
  mutate(pct_unworried = n / sum(n) * 100) %>%
  na.omit() %>%
  select(month, party, pct_unworried) %>%
  ungroup()

twentyone_opinion <- full_join(twentyone_exaggerated, twentyone_unworried, by = c("month", "party"))
twentyone_opinion <- twentyone_opinion %>%
  mutate(winner = party) %>%
  select(month, winner, pct_exaggerated, pct_unworried)

twentyone_vax <- twentyone %>%
  group_by(month, party) %>%
  count(vax) %>%
  mutate(pct_vax = n / sum(n) * 100) %>%
  filter(vax == "Vaccinated") %>%
  na.omit() %>%
  ungroup()

vax <- twentyone_vax %>%
  ggplot(aes(x = month, y = pct_vax, fill = party)) +
  geom_bar(aes(group = interaction(month, party)), stat = "identity", position = "identity", 
           color = "#000000", size = 0.05) +
  scale_fill_manual(values = c("#2C40C2", "#DF4342")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels = month.abb) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-25, 100)) +
  coord_polar(theta = "x") +
  labs(title = "US Covid Vaccination Rate by Party Affiliation (%)") +
  theme_minimal() +
  theme(text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  transition_states(states = month,  transition_length = 1, state_length = 2) +
  enter_grow() +
  shadow_mark()

vax_gif <- animate(vax, nframes = 400, res = 180, width = 1000, height = 1000, renderer = magick_renderer())

covid <- read.csv("covid.csv")
glimpse(covid)

covid <- covid %>%
  select(date, state, adult_icu_bed_covid_utilization, deaths_covid) %>%
  mutate(month = floor_date(as.Date(date), unit = "month")) %>%
  filter(month >= "2021/01/01" & date < "2022/01/01") %>%
  group_by(month, state) %>%
  summarize(pct_icu_util = mean(adult_icu_bed_covid_utilization), deaths = sum(deaths_covid))


election <- read.csv("election_president.csv")
glimpse(election)

election <- election %>%
  filter(year == 2020,
         party_simplified == "DEMOCRAT" | party_simplified == "REPUBLICAN") %>%
  mutate(state = state_po,
         winner = str_to_title(party_simplified)) %>%
  select(state, candidatevotes, winner) %>%
  group_by(state) %>%
  top_n(1, candidatevotes) %>%
  select(state, winner)


population <- read.csv("state_population.csv")
glimpse(population)

population <- population %>%
  mutate(state = fips(STATE, to = "Abbreviation"),
         population = POPESTIMATE2021) %>%
  select(state, population)


election <- inner_join(election, population, by = "state")
covid_election <- inner_join(covid, election, by = "state")

covid_election <- covid_election %>%
  group_by(month, winner) %>%
  summarize(pct_icu_util = mean(pct_icu_util) * 100,
            deaths_per_100000 = sum(deaths) / sum(population) * 100000)

covid_election <- inner_join(covid_election, twentyone_opinion, by = c("month", "winner"))
covid_election <- covid_election %>%
  pivot_longer(cols = c(pct_icu_util, deaths_per_100000, pct_exaggerated, pct_unworried), names_to = "type", values_to = "value")

facet_labs <- c("deaths_per_100000" = "Average Deaths per 100,000 Inhabitants", "pct_exaggerated" = "Believe that Covid is Exaggerated (%)",
                "pct_icu_util" = "ICU Beds Occupied by Covid Patients (%)", "pct_unworried" = "Unworried about Covid (%)")

covid_line <- covid_election %>%
  ggplot(aes(x = month, y = value, color = winner)) +
  geom_path() +
  geom_point() +
  facet_wrap(~type, scales = "free",
             labeller = as_labeller(facet_labs)) +
  scale_color_manual(values = c("#2C40C2", "#DF4342")) +
  scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01",
                                  "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                  "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
               labels = month.abb) +
  labs(title = "US Covid Data by Party Affiliation") +
  theme_minimal() +
  theme(text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  transition_reveal(along = month)

covid_line_gif <- animate(covid_line, nframes = 400, res = 180, width = 1000, height = 1000, renderer = magick_renderer())


final_gif <- image_append(c(vax_gif[1], covid_line_gif[1]))

for(i in 2:400){
  combined <- image_append(c(vax_gif[i], covid_line_gif[i]))
  final_gif <- c(final_gif, combined)
}

image_write(final_gif, "project02.gif", format = "gif")

