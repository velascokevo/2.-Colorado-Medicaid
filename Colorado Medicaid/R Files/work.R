############################################################
### Loading libraries
############################################################

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats) 
library(treemapify)
library(ggplotify)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)
library(webshot2)
library(lubridate)
library(ggh4x)
library(tidycensus)
library(tigris)
library(ggiraph)
library(htmlwidgets)
library(sf)

############################################################
### Setting up directories
############################################################

setwd("/Users/velas/Documents/Acadmic Writings/Colorado Medicaid")

############################################################
### Loading in data
############################################################

enrollment_raw <- read_csv("Data/medicaid_chip_enrollment_all.csv") # data derives from CDPHE Medicaid and CHIP cases by county by month
caseload_raw <- read_csv("Data/caseload_county.csv") # https://data.medicaid.gov/dataset/6165f45b-ca93-5bb5-9d06-db29c692a360

pop_2023 <- read_csv("Data/PopulationByAge2023.csv")

pop_2024 <- read_csv("Data/PopulationByAge2024Projections.csv")

co_county_geo <- get_acs(
    geography = "county", 
    variables = "B01003_001",
    state = "CO", 
    year = 2023,
    geometry = TRUE) |> 
  select(GEOID, NAME, geometry) |> 
  rename(county = NAME,
         geoid = GEOID)

############################################################
### Cleaning data
############################################################

# Enrollment
enrollment <- enrollment_raw |> 
  rename(state_abb = "State Abbreviation",
         state = "State Name",
         date = "Reporting Period",
         final = "Final Report",
         n_apps_medchip = "New Applications Submitted to Medicaid and CHIP Agencies",
         n_med_eligible = "Individuals Determined Eligible for Medicaid at Application",
         n_chip_eligible = "Individuals Determined Eligible for CHIP at Application",
         n_determinations = "Total Medicaid and CHIP Determinations",
         total_child_medchip = "Medicaid and CHIP Child Enrollment",
         total_medchip = "Total Medicaid and CHIP Enrollment",
         total_med = "Total Medicaid Enrollment",
         total_chip = "Total CHIP Enrollment",
         total_adult_med = "Total Adult Medicaid Enrollment") |> 
  select(state_abb, state, date, final, n_apps_medchip, n_med_eligible, n_chip_eligible, n_determinations,
         total_child_medchip, total_medchip, total_med, total_chip, total_adult_med) |> 
  filter(final == "Y",
         date != "201309") |> 
  select(-final) |> 
  mutate(total_child_med = total_med - total_adult_med,
         date = ym(date))

# Caseload
caseload <- caseload_raw |> 
  select(county, date, countm_u20, countm_o21, countm_u20, countm_total, countc_total) |> 
  mutate(county = paste0(county, " County, Colorado"),
         countc_total = if_else(countc_total == "NR", "", countc_total),
         countc_total = as.numeric(countc_total))

# Population
pop_2023 <- pop_2023 |> 
  rename(county = County) |> 
  mutate(county = paste0(county, ", Colorado")) |> 
  select(-Code)

# Caseload geo
caseload_geo <- co_county_geo |> 
  left_join(y = caseload, by = "county") |> 
  left_join(y = pop_2023, by = "county") |> 
  rename(pop_u18 = `Less than 18...4`,
         pop_total = `All ages`) |> 
  mutate(pop_o17 = pop_total - pop_u18) |> 
  select(geoid, county, date, countm_u20, countm_o21, countm_total, countc_total,
         pop_total, pop_u18, pop_o17, geometry)

############################################################
### Growth in the number of cases overtime
############################################################  

fig1 <- enrollment |> 
  filter(state_abb == "CO",
         date > 2019-01-01) |> 
  mutate(points = case_when(
    date == "2020-03-01" ~ total_med,
    date == "2023-05-01" ~ total_med,
    date == "2024-05-01" ~ total_med), 
    label_n = paste0(round(points / 100) * 100),
    label_n = if_else(is.na(points), "", scales::comma(round(points / 100) * 100))) |> 
  ggplot() +
  geom_line(mapping = aes(x = date, 
                          y = total_med),
            size = 1.5,
            color = "navyblue") +
  geom_point(mapping = aes(x = date,
                           y = points),
             size = 3.5,
             shape = 15,
             color = "navyblue") + 
  geom_text(mapping = aes(x = date,
                          y = points,
                          label = label_n),
             fontface = "italic",
             vjust = 1,
             hjust = -0.25,
             size = 4.5) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "m"),
                     breaks = seq(0, 2e6, by = 0.5e6),
                     limits = c(0.5e6, 2e6)) + 
  labs(title = "1. Colorado Medicaid Enrollees Over Time",
       subtitle = "Sharp increase during the pandemic before falling below \npre-2020 levels.",
       color = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig1.png", fig1, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Medicaid and CHIP applications vs determinations
############################################################  

fig2 <- enrollment |> 
  filter(state_abb == "CO") |> 
  select(date, n_apps_medchip, n_determinations) |> 
  ggplot() +
  geom_line(mapping = aes(x = date, 
                          y = n_apps_medchip,
                          color = "Applications"),
            size = 1.25) +
  geom_line(mapping = aes(x = date,
                          y = n_determinations,
                          color = "Determinations"),
            size = 1.25) + 
  scale_color_manual(values = c("Applications" = "red2",
                               "Determinations" = "navyblue")) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k"),
                     breaks = seq(0, 40e3, by = 10e3),
                     limits = c(0, 40e3)) + 
  labs(title = "2. Medicaid and CHIP Applications and \nDeterminations Over Time",
       subtitle = "Total applications and determinations fell during the pandemic \nbefore rising sharply afterwards.",
       color = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig2.png", fig2, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Growth in the number of cases overtime: Comparative analysis
############################################################  

fig3 <- enrollment |> 
  filter(state_abb %in% c("CO", "WI", "MO", "MD", "IN", "MA", "MN", "SC", "AL", "LA", "KY")) |> 
  ggplot() +
  geom_line(data = ~ filter(.x, state_abb != "CO"),
            mapping = aes(x = date,
                           y = total_med,
                           group = state_abb),
            size = 1.5,
            color = "grey50",
            alpha = 0.5) + 
  geom_line(data = ~ filter(.x, state_abb == "CO"),
            mapping = aes(x = date, 
                          y = total_med),
            size = 1.5,
            color = "navyblue") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "m"),
                     breaks = seq(0, 2e6, by = 0.5e6),
                     limits = c(0.5e6, 2e6)) + 
  labs(title = "3. Coloradan Medicaid Enrollees Compared to \nTen Similarly Populated States",
       subtitle = "Colorado's decline in Medicaid enrollees among the sharpest.",
       color = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))
  
ggsave("Graphs/fig3.png", fig3, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Percentage change: 2019 to 2025
############################################################  

enrollment |> 
  filter(date == "2017-06-01" | date == "2025-02-01") |> 
  select(state_abb, total_med, date) |> 
  group_by(state_abb) |> 
  mutate(pct_change = 100 * (total_med - lag(total_med)) / lag(total_med)) |> 
  filter(!is.na(pct_change)) |>
  select(-date) |> 
  mutate(fill_group = case_when(
    state_abb == "CO" ~ "CO",
    pct_change >= 0 ~ "above zero",
    pct_change < 0 ~ "below zero")) |> 
  print(n = 51)

fig4 <- enrollment |> 
  filter(date == "2017-06-01" | date == "2025-02-01") |> 
  select(state_abb, total_med, date) |> 
  group_by(state_abb) |> 
  mutate(pct_change = 100 * (total_med - lag(total_med)) / lag(total_med)) |> 
  filter(!is.na(pct_change)) |>
  select(-date) |> 
  mutate(fill_group = case_when(
    state_abb == "CO" ~ "CO",
    pct_change >= 0 ~ "above zero",
    pct_change < 0 ~ "below zero")) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = reorder(factor(state_abb), pct_change),
                         y = pct_change,
                         fill = fill_group),
           stat = "identity",
           color = "white",
           size = 0.25,
           width = 1) + 
  geom_text(mapping = aes(x = factor(state_abb),
                          y = if_else(pct_change > 0, -5, 5),
                          label = state_abb),
            size = 4,
            lineheight = 0.7,
            angle = 90) +
  geom_hline(yintercept = c(-25, 0, 25, 50, 75),
             color = "gray80",
             size = 0.25,
             linetype = 2) + 
  scale_fill_manual(values = c(
    "CO" = "goldenrod2",
    "above zero" = "red2",
    "below zero" = "navyblue")) + 
  scale_y_continuous(breaks = seq(-25, 75, by = 25),
                     limits = c(-30, 80)) + 
  labs(title = "4. States by Percentage Change in Number of Medicaid \nRecipients (2017 thru 2025)",
       subtitle = "Colorado is the state that lost the greatest relative amount of Medicaid recipients.",
       y = "Percentage change") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "black"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig4.png", fig4, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Raw number of Medicaid recipients
############################################################ 

options(scipen = 999)  

fig5 <- caseload_geo |> 
  select(county, date, countm_total, geometry) |>
  mutate(category = cut(countm_total,
                        breaks = c(0, 2500, 5000, 10000, 50000, 100000, Inf),
                        labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-50k", "50k-100k", "100k+"),
                        include.lowest = TRUE)) |> 
  filter(date == "July 2024") |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = category),
          color = "black",
          linewidth = 0.5) + 
  scale_fill_brewer(palette = "OrRd", direction = 1) + 
  labs(title = "5. Number of Medicaid Recipients by County",
       subtitle = "Higher number of recipients in urban population centers.",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig5.png", fig5, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Medicaid recipients per 1k residents
############################################################

fig6 <- caseload_geo |> 
  filter(date == "July 2024") |> 
  mutate(medicaid_perk = (countm_total / pop_total) * 1000) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = medicaid_perk),
          color = "black",
          linewidth = 0.5) + 
  scale_fill_gradient(low = "white",
                      high = "red2",
                      breaks = c(0, 250, 500),
                      labels = c(0, 250, 500), 
                      limits = c(0, 500)) + 
  labs(title = "6. Medicaid Recipients per 1,000 Residents by County",
       subtitle = "Higher rates of Medicaid recipiency in southern, rural, and plains Colorado.",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig6.png", fig6, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Medicaid under-21 recipients per 1k minor children
############################################################

fig7 <- caseload_geo |> 
  filter(date == "July 2024") |> 
  mutate(medicaidchild_perk = (countm_u20 / pop_u18) * 1000) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = medicaidchild_perk),
          color = "black",
          linewidth = 0.5) + 
  scale_fill_gradient(low = "white",
                      high = "navyblue",
                      breaks = c(0, 400, 800),
                      labels = c(0, 400, 800), 
                      limits = c(0, 803)) + 
  labs(title = "7. Under-21 Medicaid Recipients per 1,000 Residents of \nMinor Age",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig7.png", fig7, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Coverage donut charts
############################################################
  
donut_function <- function(cty) {
  
  caseload_geo |> 
    filter(date == "July 2024") |> 
    mutate(county = str_remove(county, " County, Colorado")) |> 
    filter(county == cty) |> 
    mutate(nochild_n = pop_u18 - countm_u20) |> 
    pivot_longer(cols = c(countm_u20, nochild_n), 
                 names_to = "group", 
                 values_to = "count") |> 
    mutate(pct = count / sum(count),
           label = scales::percent(pct)) |> 
    ggplot() + 
    geom_col(mapping = aes(x = 2, 
                           y = pct,
                           fill = group)) + 
    geom_text(mapping = aes(x = 1,
                            y = 1,
                            label = label)) + 
    coord_polar("y", start = 0) + 
    xlim(0.5, 2.5) + 
    theme_fivethirtyeight() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          text = element_text(size = 16),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.box.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA))
} 

donut_function("Denver")

donut_child <- function(cty) {
  
  caseload_geo |> 
    filter(date == "July 2024") |> 
    mutate(county = str_remove(county, " County, Colorado")) |> 
    filter(county == cty) |> 
    mutate(nochild_n = pop_u18 - countm_u20) |> 
    pivot_longer(cols = c(countm_u20, nochild_n), 
                 names_to = "group", 
                 values_to = "count") |> 
    mutate(group = recode(group,
                          "countm_u20" = "With Medicaid",
                          "nochild_n" = "Without Medicaid"),
           group = factor(group, levels = rev(c("With Medicaid", "Without Medicaid"))),
           pct = count / sum(count),
           label = scales::percent(pct)) |> 
    arrange(match(group, c("With Medicaid", "Without Medicaid"))) |> 
    ggplot() + 
    geom_col(mapping = aes(x = 2, 
                           y = pct,
                           fill = group)) + 
    geom_text(data = ~ filter(.x, group == "With Medicaid"),
              mapping = aes(x = 0.5,
                            y = 0.5,
                            label = label),
              size = 10,
              fontface = "bold") + 
    scale_fill_manual(values = c(
      "With Medicaid" = "goldenrod2",
      "Without Medicaid" = "white")) + 
    coord_polar("y", start = 0) + 
    xlim(0.5, 2.5) +
    theme_fivethirtyeight() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          text = element_text(size = 16),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.box.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA))
  
  }

donut_all <- function(cty) {
  
  caseload_geo |> 
    filter(date == "July 2024") |> 
    mutate(county = str_remove(county, " County, Colorado")) |> 
    filter(county == cty) |> 
    mutate(nochild_n = pop_u18 - countm_u20,
           nom_total = pop_total - countm_total) |> 
    pivot_longer(cols = c(countm_total, nom_total), 
                 names_to = "group", 
                 values_to = "count") |> 
    mutate(group = recode(group,
                          "countm_total" = "With Medicaid",
                          "nom_total" = "Without Medicaid"),
           group = factor(group, levels = rev(c("With Medicaid", "Without Medicaid"))),
           pct = count / sum(count),
           label = scales::percent(pct)) |> 
    arrange(match(group, c("With Medicaid", "Without Medicaid"))) |> 
    ggplot() + 
    geom_col(mapping = aes(x = 2, 
                           y = pct,
                           fill = group)) + 
    geom_text(data = ~ filter(.x, group == "With Medicaid"),
              mapping = aes(x = 0.5,
                            y = 0.5,
                            label = label),
              size = 13,
              fontface = "bold") + 
    scale_fill_manual(values = c(
      "With Medicaid" = "goldenrod2",
      "Without Medicaid" = "grey95")) + 
    coord_polar("y", start = 0) + 
    xlim(0.5, 2.5) +
    theme_fivethirtyeight() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          text = element_text(size = 16),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.box.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA))
  
}

# Adams
adams <- donut_all("Adams")
ggsave("Graphs/Donut/adams.png", plot = adams, width = 3, height = 3, dpi = 300)

# Alamosa
alamosa <- donut_all("Alamosa")
ggsave("Graphs/Donut/alamosa.png", plot = alamosa, width = 3, height = 3, dpi = 300)

# Arapahoe
arapahoe <- donut_all("Arapahoe")
ggsave("Graphs/Donut/arapahoe.png", plot = arapahoe, width = 3, height = 3, dpi = 300)

# Archuleta
archuleta <- donut_all("Archuleta")
ggsave("Graphs/Donut/archuleta.png", plot = archuleta, width = 3, height = 3, dpi = 300)

# Baca
baca <- donut_all("Baca")
ggsave("Graphs/Donut/baca.png", plot = baca, width = 3, height = 3, dpi = 300)

# Bent
bent <- donut_all("Bent")
ggsave("Graphs/Donut/bent.png", plot = bent, width = 3, height = 3, dpi = 300)

# Boulder
boulder <- donut_all("Boulder")
ggsave("Graphs/Donut/boulder.png", plot = boulder, width = 3, height = 3, dpi = 300)

# Broomfield
broomfield <- donut_all("Broomfield")
ggsave("Graphs/Donut/broomfield.png", plot = broomfield, width = 3, height = 3, dpi = 300)

# Chaffee
chaffee <- donut_all("Chaffee")
ggsave("Graphs/Donut/chaffee.png", plot = chaffee, width = 3, height = 3, dpi = 300)

# Cheyenne
cheyenne <- donut_all("Cheyenne")
ggsave("Graphs/Donut/cheyenne.png", plot = cheyenne, width = 3, height = 3, dpi = 300)

# Clear Creek
clear_creek <- donut_all("Clear Creek")
ggsave("Graphs/Donut/clear_creek.png", plot = clear_creek, width = 3, height = 3, dpi = 300)

# Conejos
conejos <- donut_all("Conejos")
ggsave("Graphs/Donut/conejos.png", plot = conejos, width = 3, height = 3, dpi = 300)

# Costilla
costilla <- donut_all("Costilla")
ggsave("Graphs/Donut/costilla.png", plot = costilla, width = 3, height = 3, dpi = 300)

# Crowley
crowley <- donut_all("Crowley")
ggsave("Graphs/Donut/crowley.png", plot = crowley, width = 3, height = 3, dpi = 300)

# Custer
custer <- donut_all("Custer")
ggsave("Graphs/Donut/custer.png", plot = custer, width = 3, height = 3, dpi = 300)

# Delta
delta <- donut_all("Delta")
ggsave("Graphs/Donut/delta.png", plot = delta, width = 3, height = 3, dpi = 300)

# Denver
denver <- donut_all("Denver")
ggsave("Graphs/Donut/denver.png", plot = denver, width = 3, height = 3, dpi = 300)

# Dolores
dolores <- donut_all("Dolores")
ggsave("Graphs/Donut/dolores.png", plot = dolores, width = 3, height = 3, dpi = 300)

# Douglas
douglas <- donut_all("Douglas")
ggsave("Graphs/Donut/douglas.png", plot = douglas, width = 3, height = 3, dpi = 300)

# Eagle
eagle <- donut_all("Eagle")
ggsave("Graphs/Donut/eagle.png", plot = eagle, width = 3, height = 3, dpi = 300)

# El Paso
el_paso <-donut_all("El Paso")
ggsave("Graphs/Donut/el_paso.png", plot = el_paso, width = 3, height = 3, dpi = 300)

# Elbert
elbert <- donut_all("Elbert")
ggsave("Graphs/Donut/elbert.png", plot = elbert, width = 3, height = 3, dpi = 300)

# Fremont
fremont <- donut_all("Fremont")
ggsave("Graphs/Donut/fremont.png", plot = fremont, width = 3, height = 3, dpi = 300)

# Garfield
garfield <- donut_all("Garfield")
ggsave("Graphs/Donut/garfield.png", plot = garfield, width = 3, height = 3, dpi = 300)

# Gilpin
gilpin <- donut_all("Gilpin")
ggsave("Graphs/Donut/gilpin.png", plot = gilpin, width = 3, height = 3, dpi = 300)

# Grand
grand <- donut_all("Grand")
ggsave("Graphs/Donut/grand.png", plot = grand, width = 3, height = 3, dpi = 300)

# Gunnison
gunnison <- donut_all("Gunnison")
ggsave("Graphs/Donut/gunnison.png", plot = gunnison, width = 3, height = 3, dpi = 300)

# Hinsdale
hinsdale <- donut_all("Hinsdale")
ggsave("Graphs/Donut/hinsdale.png", plot = hinsdale , width = 3, height = 3, dpi = 300)

# Huerfano
huerfano <- donut_all("Huerfano")
ggsave("Graphs/Donut/huerfano.png", plot = huerfano, width = 3, height = 3, dpi = 300)

# Jackson
jackson <- donut_all("Jackson")
ggsave("Graphs/Donut/jackson.png", plot = jackson, width = 3, height = 3, dpi = 300)

# Jefferson
jefferson <- donut_all("Jefferson")
ggsave("Graphs/Donut/jefferson.png", plot = jefferson, width = 3, height = 3, dpi = 300)

# Kiowa
kiowa <- donut_all("Kiowa")
ggsave("Graphs/Donut/kiowa.png", plot = kiowa, width = 3, height = 3, dpi = 300)

# Kit Carson
kit_carson <- donut_all("Kit Carson")
ggsave("Graphs/Donut/kit_carson.png", plot = kit_carson, width = 3, height = 3, dpi = 300)

# La Plata
la_plata <- donut_all("La Plata")
ggsave("Graphs/Donut/la_plata.png", plot = la_plata, width = 3, height = 3, dpi = 300)

# Lake
lake <- donut_all("Lake")
ggsave("Graphs/Donut/lake.png", plot = lake, width = 3, height = 3, dpi = 300)

# Larimer
larimer <- donut_all("Larimer")
ggsave("Graphs/Donut/larimer.png", plot = larimer, width = 3, height = 3, dpi = 300)

# Las Animas
las_animas <- donut_all("Las Animas")
ggsave("Graphs/Donut/las_animas.png", plot = las_animas, width = 3, height = 3, dpi = 300)

# Lincoln
lincoln <- donut_all("Lincoln")
ggsave("Graphs/Donut/lincoln.png", plot = lincoln, width = 3, height = 3, dpi = 300)

# Logan
logan <- donut_all("Logan")
ggsave("Graphs/Donut/logan.png", plot = logan, width = 3, height = 3, dpi = 300)

# Mesa
mesa <- donut_all("Mesa")
ggsave("Graphs/Donut/mesa.png", plot = mesa, width = 3, height = 3, dpi = 300)

# Mineral
mineral <- donut_all("Mineral")
ggsave("Graphs/Donut/mineral.png", plot = mineral, width = 3, height = 3, dpi = 300)

# Moffat
moffat <- donut_all("Moffat")
ggsave("Graphs/Donut/moffat.png", plot = moffat, width = 3, height = 3, dpi = 300)

# Montezuma
montezuma <- donut_all("Montezuma")
ggsave("Graphs/Donut/montezuma.png", plot = montezuma, width = 3, height = 3, dpi = 300)

# Montrose
montrose <- donut_all("Montrose")
ggsave("Graphs/Donut/montrose.png", plot = montrose, width = 3, height = 3, dpi = 300)

# Morgan
morgan <- donut_all("Morgan")
ggsave("Graphs/Donut/morgan.png", plot = morgan, width = 3, height = 3, dpi = 300)

# Otero
otero <- donut_all("Otero")
ggsave("Graphs/Donut/otero.png", plot = otero, width = 3, height = 3, dpi = 300)

# Ouray
ouray <- donut_all("Ouray")
ggsave("Graphs/Donut/ouray.png", plot = ouray, width = 3, height = 3, dpi = 300)

# Park
park <- donut_all("Park")
ggsave("Graphs/Donut/park.png", plot = park, width = 3, height = 3, dpi = 300)

# Phillips
phillips <- donut_all("Phillips")
ggsave("Graphs/Donut/phillips.png", plot = phillips, width = 3, height = 3, dpi = 300)

# Pitkin
pitkin <- donut_all("Pitkin")
ggsave("Graphs/Donut/pitkin.png", plot = pitkin, width = 3, height = 3, dpi = 300)

# Prowers
prowers <- donut_all("Prowers")
ggsave("Graphs/Donut/prowers.png", plot = prowers, width = 3, height = 3, dpi = 300)

# Pueblo
pueblo <- donut_all("Pueblo")
ggsave("Graphs/Donut/pueblo.png", plot = pueblo, width = 3, height = 3, dpi = 300)

# Rio Blanco
rio_blanco <- donut_all("Rio Blanco")
ggsave("Graphs/Donut/rio_blanco.png", plot = rio_blanco, width = 3, height = 3, dpi = 300)

# Rio Grande
rio_grande <- donut_all("Rio Grande")
ggsave("Graphs/Donut/rio_grande.png", plot = rio_grande, width = 3, height = 3, dpi = 300)

# Routt
routt <- donut_all("Routt")
ggsave("Graphs/Donut/routt.png", plot = routt, width = 3, height = 3, dpi = 300)

# Saguache
saguache <- donut_all("Saguache")
ggsave("Graphs/Donut/saguache.png", plot = saguache, width = 3, height = 3, dpi = 300)

# San Juan
san_juan <- donut_all("San Juan")
ggsave("Graphs/Donut/san_juan.png", plot = san_juan, width = 3, height = 3, dpi = 300)

# San Miguel
san_miguel <- donut_all("San Miguel")
ggsave("Graphs/Donut/san_miguel.png", plot = san_miguel, width = 3, height = 3, dpi = 300)

# Sedgwick
sedgwick <- donut_all("Sedgwick")
ggsave("Graphs/Donut/sedgwick.png", plot = sedgwick, width = 3, height = 3, dpi = 300)

# Summit
summit <- donut_all("Summit")
ggsave("Graphs/Donut/summit.png", plot = summit, width = 3, height = 3, dpi = 300)

# Teller
teller <- donut_all("Teller")
ggsave("Graphs/Donut/teller.png", plot = teller, width = 3, height = 3, dpi = 300)

# Washington
washington <- donut_all("Washington")
ggsave("Graphs/Donut/washington.png", plot = washington, width = 3, height = 3, dpi = 300)

# Weld
weld <- donut_all("Weld")
ggsave("Graphs/Donut/weld.png", plot = weld, width = 3, height = 3, dpi = 300)

# Yuma
yuma <- donut_all("Yuma")
ggsave("Graphs/Donut/yuma.png", plot = yuma, width = 3, height = 3, dpi = 300)

### GT Table

donut_dir <- file.path(getwd(), "Graphs/Donut")

caseload_geo |> 
  filter(date == "July 2024") |> 
  st_drop_geometry() |> 
  mutate(county = str_remove(county, " County, Colorado"),
         nochild_n = pop_u18 - countm_u20,
         nom_total = pop_total - countm_total,
         pop_total = round(pop_total, -1),
         countm_total = round(countm_total, -1),
         class = case_when(
           county %in% c("Adams", "Arapahoe", "Boulder", "Broomfield", "Denver", "Douglas",
                         "El Paso", "Jefferson", "Larimer", "Mesa", "Pueblo", "Teller", "Weld") ~ "Urban",
           county %in% c("Archuleta", "Chaffee", "Eagle", "Grand", "Gunnison", "La Plata", "Lake",
                         "Ouray", "Pitkin", "Routt", "San Juan", "San Miguel", "Summit") ~ "Rural Resort",
           TRUE ~ "Rural")) |>
  select(county, pop_total, countm_total, class) |> 
  arrange(-desc(county)) |> 
  tibble(donut = c("/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/adams.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/alamosa.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/arapahoe.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/archuleta.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/baca.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/bent.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/boulder.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/broomfield.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/chaffee.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/cheyenne.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/clear_creek.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/conejos.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/costilla.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/crowley.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/custer.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/delta.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/denver.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/dolores.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/douglas.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/eagle.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/el_paso.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/elbert.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/fremont.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/garfield.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/gilpin.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/grand.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/gunnison.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/hinsdale.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/huerfano.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/jackson.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/jefferson.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/kiowa.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/kit_carson.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/la_plata.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/lake.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/larimer.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/las_animas.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/lincoln.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/logan.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/mesa.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/mineral.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/moffat.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/montezuma.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/montrose.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/morgan.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/otero.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/ouray.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/park.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/phillips.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/pitkin.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/prowers.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/pueblo.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/rio_blanco.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/rio_grande.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/routt.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/saguache.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/san_juan.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/san_miguel.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/sedgwick.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/summit.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/teller.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/washington.png", 
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/weld.png",
                   "/Users/velas/Documents/Acadmic Writings/Colorado Medicaid/Graphs/Donut/yuma.png")) |> 
  rename(County = county,
         `Total Population` = pop_total,
         `Total Medicaid Recipients` = countm_total,
         `Pct. with Medicaid` = donut,
         Classification = class) |> 
  select(County, `Total Population`, `Total Medicaid Recipients`, `Pct. with Medicaid`, Classification) |> 
  gt() |> 
  tab_header(title = md("8. Counties by Percentage Covered by Medicaid")) |> 
  gt_img_rows(columns = `Pct. with Medicaid`, height = 75) |> 
  fmt_number(columns = c(`Total Population`, `Total Medicaid Recipients`),
             decimals = 0,
             use_seps = TRUE) |> 
  opt_vertical_padding(scale = 0) |> 
  cols_width(everything() ~ px(110)) |> 
  opt_table_font(font = list(google_font("Lato"),
                             default_fonts())) |>
  opt_align_table_header(align = "left") |> 
  tab_style(style = cell_text(size = px(16),
                              weight = "bold"),
            locations = cells_column_labels(everything())) |> 
  tab_style(style = cell_text(size = px(26),
                              weight = "bold"),
            locations = cells_title(groups = "title")) |> 
  tab_style(style = cell_text(size = px(20)),
            locations = cells_title(groups = "subtitle")) |> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = County)) |> 
  gtsave("Graphs/fig8.png")








