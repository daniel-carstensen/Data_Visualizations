## Data Visualization (GOVT16-QSS17) Spring 2022
## Intermediate ggplot2 & Customization
##
## Name: Daniel Carstensen
## Date: April 25 - May 2, 2022

## 1.

install.packages("HistData")
library(HistData)

install.packages("gridExtra")
library(gridExtra)

nightingale <- Nightingale

## a)
head(nightingale)
tail(nightingale)
glimpse(nightingale)

## b) - d)
nightingale_1 <- nightingale %>%
  filter(Date > "1854-03-01" & Date < "1855-04-01") %>%
  mutate(Month = factor(Month, levels = month.abb, labels =
                          c("JANUARY 1855", "FEBRUARY", "MARCH 1855", "APRIL\n1854",
                            "\n\nMAY", "\n\nJUNE", "JULY", "AUGUST", "SEPTEMBER",
                            "OCTOBER", "NOVEMBER", "DECEMBER"))) %>%
  mutate(Month = fct_relevel(Month, "JULY", "AUGUST", "SEPTEMBER", "OCTOBER",
                             "NOVEMBER", "DECEMBER", "JANUARY 1855", "FEBRUARY",
                             "MARCH 1855", "APRIL\n1854", "\n\nMAY", "\n\nJUNE")) %>%
  mutate(Angle = c(80, 45, 10, -15, -45, -75, -105, -130, -168, -195, -222, 105)) %>%
  pivot_longer(names_to = "Cause",
               values_to = "Rate",
               Disease.rate:Other.rate) %>%
  mutate(Sqrt.Rate = sqrt(Rate))

Labels_1 <- nightingale_1 %>%
  group_by(Month) %>%
  filter(Sqrt.Rate == max(Sqrt.Rate)) %>%
  mutate(Sqrt.Rate = replace(Sqrt.Rate, Sqrt.Rate < 3, 6))

period_1 <- nightingale_1 %>%
  group_by(Cause) %>%
  ggplot(aes(x = Month, y = Sqrt.Rate, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "identity", color = "#000000", size = 0.05, alpha = 0.5) +
  geom_text(data = Labels_1, aes(label = Month, angle = Angle), position = "stack",
            vjust = -1, size = 1.5, family = "Gill Sans", fontface = "bold") +
  coord_polar(theta = "x") +
  scale_fill_manual(values = c("#93a7bf", "#000000", "#d17a6b")) +
  labs(title = "1.\nAPRIL 1854 TO MARCH 1855") +
  theme_void() +
  theme(plot.title = element_text(family = "Baskerville", face = "bold", hjust = 0.5,
                                  vjust = -75, size = 6)) +
  theme(text = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.title = element_text()) +
  theme(plot.margin = unit(c(-6.5, -2, -2, 0), "cm"))


nightingale_2 <- nightingale %>%
  filter(Date > "1855-03-01" & Date < "1856-04-01") %>%
  mutate(Month = factor(Month, levels = month.abb, labels =
                          c("JANUARY\n1856", "\n\nFEBRUARY", "\n\nMARCH", "APRIL 1855",
                            "MAY", "JUNE", "JULY", "AUGUST", "\n\nSEPTEMBER",
                            "\n\nOCTOBER", "\n\nNOVEMBER", "\n\nDECEMBER"))) %>%
  mutate(Month = fct_relevel(Month, "JULY", "AUGUST", "\n\nSEPTEMBER", "\n\nOCTOBER",
                             "\n\nNOVEMBER", "\n\nDECEMBER", "JANUARY\n1856", "\n\nFEBRUARY",
                             "\n\nMARCH", "APRIL 1855", "MAY", "JUNE")) %>%
  mutate(Angle = c(80, 45, 10, -15, -45, -75, -105, -130, -168, -195, -222, 105)) %>%
  pivot_longer(names_to = "Cause",
               values_to = "Rate",
               Disease.rate:Other.rate) %>%
  mutate(Sqrt.Rate = sqrt(Rate))

Labels_2 <- nightingale_2 %>%
  group_by(Month) %>%
  filter(Sqrt.Rate == max(Sqrt.Rate)) %>%
  mutate(Sqrt.Rate = replace(Sqrt.Rate, Sqrt.Rate < 10, 8))

period_2 <- nightingale_2 %>%
  group_by(Cause) %>%
  ggplot(aes(x = Month, y = Sqrt.Rate, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "identity", color = "#000000", size = 0.05, alpha = 0.5) +
  geom_text(data = Labels_2, aes(label = Month, angle = Angle), position = "stack",
            vjust = -1, size = 1.5, family = "Gill Sans", fontface = "bold") +
  coord_polar(theta = "x") +
  scale_fill_manual(values = c("#93a7bf", "#000000", "#d17a6b")) +
  labs(title = "2.\nAPRIL 1855 TO MARCH 1856") +
  theme_void() +
  theme(plot.title = element_text(family = "Baskerville", face = "bold", hjust = 0.5,
                                  vjust = -5, size = 6)) +
  theme(text = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.title = element_text())


explain <- ggplot() +
  annotate("text", 0, 0, size = 2.5,
           label = "The Areas of the blue, red, & black wedges are each measured from\n   the centre as the common vertex.\nThe blue wedges measured from the centre of the circle represent area\n   for area the deaths from Preventible or Mitigable Zymotic diseases; the\n   red wedges measured from the centre the deaths from wounds; & the\n   black wedges measured from the centre the deaths from all other causes.\nThe black line across the red triangle in Nov 1854 marks the boundary\n   of the deaths from all other causes during the month.\nIn October 1854, & April 1855; the black area coincides with the red;\n   in January & February 1856 the blue coincides with the black.\nThe entire areas may be compared by following the blue, the red & the\n   black lines enclosing them.",
           family = "Didot", fontface = "italic") + 
  theme_void()

## e)
title = text_grob("DIAGRAM OF THE CAUSES OF MORTALITY\nIN THE ARMY IN THE EAST",
                  face = "bold", family = "Didot", size = 8)
grid.arrange(arrangeGrob(period_2, explain, nrow = 2, heights = c(1.7, 1)), arrangeGrob(period_1), ncol = 2,
             top = title,
             widths = c(1,1.5))


## 2.
wiid <- read_xlsx("WIID_31MAY2021_0.xlsx")

high_income_pie <- wiid %>%
  group_by(region_un) %>%
  filter(incomegroup == "High income") %>%
  count(incomegroup) %>%
  ungroup() %>%
  mutate(proportion = n/sum(n) * 100)
  ggplot(aes(x = factor(1), fill = region_un, y = n)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = paste0(round(proportion, 2), "%"), x = 1.6),
            position = position_stack(vjust = 0.5), size = 3, box.padding = 0.01) +
  coord_polar(theta = "y") +
  labs(title = "High Income",fill = "UN Regions") +
  scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

low_income_pie <- wiid %>%
  group_by(region_un) %>%
  filter(incomegroup == "Low income") %>%
  count(incomegroup) %>%
  ungroup() %>%
  mutate(proportion = n/sum(n) * 100) %>%
  ggplot(aes(x = factor(1), fill = region_un, y = n)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = paste0(round(proportion, 2), "%"), x = 1.6),
                  position = position_stack(vjust = 0.5), size = 3, box.padding = 0.01) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")) +
  labs(title = "Low Income") +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(text = element_text(size = 8)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(low_income_pie, high_income_pie, ncol = 2, widths = c(1, 1.3),
             top = "Proportion of Low Income and High Income Countries by UN Region")

## 3.
nordic <- wiid %>%
  filter(country == c("Denmark", "Finland", "Iceland", "Norway", "Sweden"))

nordic %>%
  filter(year > 1964) %>%
  group_by(year) %>%
  mutate(mean_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_gini, col = country, alpha = country == "Sweden")) +
  geom_point() +
  geom_smooth(size = 0, span = 0.6, alpha = 0.1) +
  stat_smooth(geom = "line", size = 0.8) +
  scale_color_manual(values = c("#ffa600", "#5886a5", "#F10000", "#4B8700", "#004B87")) +
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  labs(title = "Gini Score of Sweden 1965 - 2019",
       y = "Gini Score") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())


## 4.
wiid %>%
  mutate(log.gdppc = log(gdp)) %>%
  ggplot(aes(x = log.gdppc, y = gini, fill = stat(count))) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colors = c("#0511A3", "#EBF03F", "#FB0505")) +
  labs(title = "Gini Score and GDP per Capita",
       x = "GDP per Capita",
       y = "Gini Score",
       fill = "Number of\nObservations") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.95, 0.2)) +
  theme(legend.background = element_rect(fill = "#FFFFFF", size = 0.5))


## 5.
wiid %>%
  mutate(log.gdppc = log(gdp)) %>%
  ggplot(aes(x = log.gdppc, y = gini)) +
  geom_point(alpha = 0.5, size = 0.6, color = "#CCCCCC") +
  stat_density2d(aes(fill = stat(level)), geom = "polygon", alpha = 0.8) +
  scale_fill_gradientn(colors = c("#0511A3", "#EBF03F", "#FB0505")) +
  labs(title = "Gini Score and GDP per Capita",
       x = "GDP per Capita",
       y = "Gini Score",
       fill = "Density\nValue") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

## 6.
wiid %>%
  filter(year > 1939) %>%
  ggplot(aes(x = year, y = gini)) +
  geom_point(data = select(filter(wiid, year > 1939), - region_un), size = 0.3, col = "lightgrey") +
  geom_point(aes(col = region_un), alpha = 0.8, size = 0.6) +
  facet_grid(rows = vars(region_un)) +
  scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")) +
  labs(title = "Gini Scores across UN Regions",
       y = "Gini Score") +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size = 8)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))


## 7.
wiid$incomegroup <- factor(wiid$incomegroup , 
                           levels = c("High income", "Upper middle income", 
                                      "Lower middle income", "Low income"),
                           ordered = TRUE)

wiid %>%
  mutate(log.gdppc = log(gdp)) %>%
  ggplot(aes(x = log.gdppc, y = gini)) +
  geom_point(alpha = 0.5, size = 0.6, color = "#CCCCCC") +
  stat_density2d(aes(fill = stat(level)), geom = "polygon", alpha = 0.8) +
  facet_grid(rows = vars(region_un), cols = vars(incomegroup)) +
  scale_fill_gradientn(colors = c("#0511A3", "#EBF03F", "#FB0505")) +
  labs(title = "Gini Score and GDP per Capita",
       x = "GDP per Capita",
       y = "Gini Score",
       fill = "Density\nValue") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))
