covid <- read.csv("covid.csv")
glimpse(covid)

case_loads_1 <- covid %>%
  mutate(vax_num = case_when(
    str_detect(vaxcount, "One") ~ 1,
    str_detect(vaxcount, "Two") ~ 2,
    str_detect(vaxcount, "Three") ~ 3,
    str_detect(vaxcount, regex("Four.", dotall = TRUE)) ~ 3,
    str_detect(vaxcount, "Refused") ~ NA_real_,
    str_detect(vaxcount, "") ~ 0)) %>%
  filter(party != "Don't Know",
         party != "Refused",
         party != "Or what?") %>%
  select(party, cases_avg_per_100k, vax_num) %>%
  group_by(party, vax_num) %>%
  add_count(party) %>%
  summarize(mean_cases_avg_per_100k = mean(cases_avg_per_100k, na.rm = TRUE))

case_loads_2 <- covid %>%
  mutate(vax_num = case_when(
    str_detect(vaxcount, "One") ~ 1,
    str_detect(vaxcount, "Two") ~ 2,
    str_detect(vaxcount, "Three") ~ 3,
    str_detect(vaxcount, regex("Four.", dotall = TRUE)) ~ 3,
    str_detect(vaxcount, "Refused") ~ NA_real_,
    str_detect(vaxcount, "") ~ 0)) %>%
  filter(party != "Don't Know",
         party != "Refused",
         party != "Or what?",
         recage2 != "DK/RF") %>%
  select(party, cases_avg_per_100k, recage2, vax_num) %>%
  group_by(party, recage2, vax_num) %>%
  add_count(party) %>%
  summarize(mean_cases_avg_per_100k = mean(cases_avg_per_100k, na.rm = TRUE))

covid %>%
  mutate(vax_num = case_when(
    str_detect(vaxcount, "One") ~ 1,
    str_detect(vaxcount, "Two") ~ 2,
    str_detect(vaxcount, "Three") ~ 3,
    str_detect(vaxcount, regex("Four.", dotall = TRUE)) ~ 3,
    str_detect(vaxcount, "Refused") ~ NA_real_,
    str_detect(vaxcount, "") ~ 0)) %>%
  filter(party != "Don't Know",
         party != "Refused",
         party != "Or what?") %>%
  filter(recage2 != "DK/RF") %>%
  mutate(party = factor(party, levels = c("Republican", "Independent", "Democrat"))) %>%
  select(vax_num, recage2, party) %>%
  group_by(party, recage2) %>%
  add_count(party) %>%
  add_count(vax_num) %>%
  mutate(vax_perc = nn / n * 100) %>%
  group_by(party, recage2, vax_num) %>%
  summarise(vax_perc = mean(vax_perc))  %>%
  ggplot(aes(x = vax_num, y = vax_perc)) +
  geom_line(data = case_loads_1, aes(x = vax_num, y = mean_cases_avg_per_100k, color = "Caseload"), linetype = "dashed", inherit.aes = FALSE) +
  scale_color_manual(name = "Average Covid Cases per 100K", values = c("Caseload" = "grey40")) +
  geom_bar(aes(fill = party, alpha = recage2), stat = "identity", position = "dodge") +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Avg Cases per 100k")) +
  geom_text(aes(label = round(vax_perc), group = recage2), vjust = -1, size = 2,
            position = position_dodge(width = 0.9)) +
  scale_alpha_manual(values = c(0.3, 0.5, 0.75, 1)) +
  facet_grid(cols = vars(party)) +
  labs(title = "Covid Vaccination Rates and Covid Case Rates\n across Party Affiliation and Age Groups",
       x = "Number of Vaccinations",
       y = "Percentage, in %",
       caption = "Henry J. Kaiser Family Foundation. (2022). Kaiser Family Foundation Poll: February 2022 COVID-19 Vaccine Monitor (Version 2) [Dataset].\nCornell University, Ithaca, NY: Roper Center for Public Opinion Research. doi:10.25940/ROPER-31119366",
       fill = "Party Affiliation",
       color = "Covid Cases",
       alpha = "Age Groups") +
  scale_fill_manual(values = c("#DE0100", "#F2B000", "#0015BC")) +
  theme_minimal() +
  theme(text = element_text(size = 8),
        title = element_text(size = 10),
        strip.text.x = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
