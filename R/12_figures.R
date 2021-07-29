#### 12 FIGURES ################################################################

source("R/01_startup.R")

property <- qread("output/property.qs", nthreads = availableCores())
daily <- qread("output/daily.qs", nthreads = availableCores())
daily_all <- qread("output/daily_all.qs", nthreads = availableCores())
GH <- qread("output/GH.qs", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Figure 1 Active daily listings ------------------------------------------

displayed_listings_all <-
  daily_all %>% 
  filter(housing) %>% 
  count(date, min_30 = minimum_stay >= 30, blocked = status == "B") %>% 
  group_by(min_30, blocked) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  group_by(date, min_30) %>% 
  mutate(n = if_else(blocked == TRUE, sum(n), n)) %>% 
  ungroup() %>% 
  mutate(blocked = if_else(blocked == TRUE, "Displayed listings", 
                           "Active listings"))

figure_1 <- 
  displayed_listings_all |> 
  mutate(label = if_else(date == "2018-01-01", case_when(
    min_30 & blocked == "Active listings" ~ "Active LTR listings",
    !min_30 & blocked == "Active listings" ~ "Active STR listings",
    min_30 & blocked == "Displayed listings" ~ "Displayed LTR listings",
    !min_30 & blocked == "Displayed listings" ~ "Displayed STR listings"), 
    NA_character_)) |>
  ggplot(aes(date, n, colour = blocked, linetype = min_30)) +
  geom_vline(aes(xintercept = as.Date("2019-11-01")), linetype = 3) +
  annotate("text", x = as.Date("2019-12-10"), y = 20000, hjust = "left",
           label = "STR regulations \nintroduced", 
           family = "Futura Condensed") +
  geom_line(lwd = 1.5) +
  geom_label(aes(label = label), family = "Futura Condensed", size = 3) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2015-01-01"), NA)) +
  scale_linetype_discrete(name = NULL, labels = c("STR", "LTR")) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)]) +
  scale_size_manual(values = c("All listings" = 1.5, "Entire home/apt" = 0.75,
                               "Private room" = 0.75, "Shared room" = 0.75),
                    guide = "none") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura")
        )

ggsave("output/figures/figure_1.pdf", plot = figure_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_1.pdf")


# Figure 2 Active listings as a share of dwellings ------------------------

active_nbhd <-
  daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2021)) |> 
  count(nbhd, date) |> 
  group_by(nbhd, year = year(date)) |> 
  summarize(n = mean(n, na.rm = TRUE), .groups = "drop") |> 
  left_join(nbhd) |> 
  mutate(percentage = n / dwellings, n = round(n, digit = -1)) |>  
  st_as_sf() |> 
  select(nbhd, year, n, dwellings, percentage, geometry)

figure_2 <- 
  active_nbhd |> 
  ggplot() +
  geom_sf(data = state, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = percentage), size = 0.3, colour = "white") +
  geom_sf(data = water, fill = "white", colour = "transparent") +
  scale_fill_stepsn(colours = col_palette[c(3, 3, 4, 1, 2, 2)], 
                    na.value = "grey80", limits = c(0, 0.04), 
                    oob = scales::squish, labels = scales::percent) +
  facet_wrap(vars(year), nrow = 1) +
  guides(fill = guide_colourbar(title = "STRs/\ndwelling", title.vjust = 1)) + 
  gg_bbox(active_nbhd) +
  theme_void() +
  theme(text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(family = "Futura", size = 5),
        panel.border = element_rect(colour = "white", size = 2),
        legend.position = "right")

ggsave("output/figures/figure_2.pdf", plot = figure_2, width = 8, 
       height = 5.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2.pdf")


# Figure 3 YOY listing and revenue growth rates ---------------------------

daily_variation <- 
  daily %>% 
  filter(housing, status != "B", date >= "2015-01-01", date != "2020-02-29") %>% 
  group_by(date) %>% 
  summarize("Listings" = n(), Revenue = sum(price[status == "R"])) %>% 
  mutate(across(where(is.numeric), 
                function(x) slide_dbl(x, ~{(.x[366] - .x[1]) / .x[1]}, 
                                      .before = 365, .complete = FALSE))) %>% 
  pivot_longer(-date, names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  mutate(value = slide_dbl(value, mean, .before = 13, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(date >= "2017-01-01")

figure_3 <- 
  daily_variation |> 
  mutate(label = if_else(date == "2018-01-01", var, NA_character_)) |> 
  ggplot(aes(date, value, colour = var)) +
  geom_vline(aes(xintercept = as.Date("2019-11-01")), linetype = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(lwd = 1) +
  geom_label(aes(label = label), family = "Futura Condensed", size = 3) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(-1, NA), 
                     labels = scales::percent) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura")
  )

ggsave("output/figures/figure_3.pdf", plot = figure_3, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3.pdf")


# Figure 4 Multilistings --------------------------------------------------

ML <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price[status == "R" & multi], na.rm = TRUE) / 
              sum(price[status == "R"], na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), slide_dbl, mean, .before = 13)) %>% 
  pivot_longer(c(Listings, Revenue), names_to = "var", values_to = "value")

figure_4 <- 
  ML |> 
  mutate(label = if_else(date == "2018-01-01", var, NA_character_)) |> 
  ggplot(aes(date, value, colour = var)) +
  geom_line(lwd = 1) +
  geom_vline(aes(xintercept = as.Date("2019-11-01")), linetype = 3) +
  geom_label(aes(label = label), family = "Futura Condensed", size = 3) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_y_continuous(name = NULL, 
                     label = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura")
  )

ggsave("output/figures/figure_4.pdf", plot = figure_4, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4.pdf")


# Figure 5 Comparison of listing change -----------------------------------

bookings_comparison <- 
  daily |> 
  filter(housing, date >= "2019-01-01", status == "R") |> 
  mutate(city = "Los Angeles") |> 
  bind_rows(other_bookings) |> 
  select(property_ID, date, city) |> 
  count(city, date) |> 
  mutate(year = year(date), month = month(date)) |> 
  group_by(city) |> 
  mutate(R_june = 100 * n / mean(n[year == 2019 & month == 6]),
         R_feb = 100 * n / mean(n[year == 2020 & month == 2])) |> 
  ungroup()

figure_5 <-
  bookings_comparison |> 
  group_by(city) |> 
  mutate(across(c(R_june, R_feb), slide_dbl, mean, .before = 13)) |> 
  ungroup() |> 
  pivot_longer(c(R_june, R_feb), names_to = "index") |> 
  filter(year >= 2020 | index == "R_june") |> 
  mutate(index = if_else(index == "R_june", "2019-06 = 100", 
                         "2020-02 = 100")) |> 
  mutate(label = case_when(
    index == "2020-02 = 100" & city == "Los Angeles" & date == "2020-09-01" ~ 
      city,
    index == "2020-02 = 100" & city == "New York" & date == "2020-05-01" ~ city,
    index == "2020-02 = 100" & city == "San Francisco" & date == "2021-02-01" ~ 
      city,
    index == "2019-06 = 100" & city == "Los Angeles" & date == "2019-11-01" ~ 
      city,
    index == "2019-06 = 100" & city == "New York" & date == "2019-02-01" ~ city,
    index == "2019-06 = 100" & city == "San Francisco" & date == "2020-03-01" ~
      city,
    TRUE ~ NA_character_)) |> 
  ggplot(mapping = aes(date,value, group = city, colour = city)) +
  geom_line(aes(size = city)) +
  geom_label(aes(label = label), family = "Futura Condensed", size = 3) +
  facet_wrap(vars(index), nrow = 1, scales = "free_x") +
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL) +
  scale_colour_manual(values = c("Los Angeles" = col_palette[5], 
                                 "New York" = "grey50", 
                                 "San Francisco" = "grey50")) +
  scale_size_manual(values = c("Los Angeles" = 1.5, "New York" = 0.5, 
                               "San Francisco" = 0.5)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura")
  )

ggsave("output/figures/figure_5.pdf", plot = figure_5, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5.pdf")


# Figure 6 Regulatory compliance ------------------------------------------

# remotes::install_github("hrbrmstr/waffle", force = TRUE)
library(waffle)

# Get final_reg from 11_analysis.R
figure_6 <-
  final_reg |> 
  mutate(across(c(pct, listings), ~if_else(
    category == "exempt", 
    sum(.x[category %in% c("missing_at_border", "exempt")]), .x))) |> 
    filter(category != "missing_at_border") |> 
  mutate(status = c("Illegal: missing #",
                    "Legal: exempt or ambiguous",
                    "Illegal: fake or duplicate #",
                    "Illegal: valid #, multilisting or > 183 nights",
                    "Legal: valid #"),
         legal = c(FALSE, TRUE, FALSE, FALSE, TRUE)) |> 
  mutate(status = factor(status, levels = c(
    "Illegal: missing #", "Illegal: fake or duplicate #",
    "Illegal: valid #, multilisting or > 183 nights", 
    "Legal: valid #", "Legal: exempt or ambiguous"))) |> 
  arrange(desc(status)) |> 
  select(status, legal, listings) |> 
  ggplot() +
  coord_equal() +
  geom_waffle(aes(fill = status, values = listings, colour = legal),
              height = 0.9, width = 0.9, n_rows = 10, size = 0.6, flip = TRUE, 
              radius = unit(4, "pt"), make_proportional = TRUE) +
  scale_fill_manual(name = "Registration status",
                    values = c(alpha(col_palette[c(1, 4, 2)], 0.8),
                               col_palette[c(3, 5)]),
                    guide = guide_legend(override.aes = list(
                      colour = c(rep("transparent", 3), "black", "black")))) +
  scale_colour_manual(values = c("transparent", "black"), guide = FALSE) +
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figures/figure_6.pdf", plot = figure_6, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_6.pdf")


# Figure 7 Housing loss ---------------------------------------------------

FREH_total <- 
  daily %>% 
  filter(housing, date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH_3), sum)) %>%
  filter(day(date) == 1)

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH = sum(housing_units)) %>%
  mutate(GH = slide_dbl(GH, mean, .before = 29))

housing_loss <-
  FREH_total %>%
  select(date, FREH_3) %>% 
  left_join(GH_total, by = "date") %>%
  rename(`Entire home/apt` = FREH_3, `Private room` = GH) %>%
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") %>% 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

# Housing loss graph
figure_7 <- 
  housing_loss %>% 
  ggplot(aes(date, `Housing units`, fill = `Listing type`)) +
  geom_vline(aes(xintercept = as.Date("2019-11-01")), linetype = 3) +
  geom_col(lwd = 0) +
  scale_fill_manual(values = col_palette[c(1, 5)]) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"),
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_7.pdf", plot = figure_7, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_7.pdf")


# Figure 8 Housing loss by neighborhood -----------------------------------

FREH_nbhd_map <- 
  daily %>% 
  filter(housing, (date == "2019-05-01" | date == "2021-05-01")) %>% 
  group_by(nbhd, year = year(date)) %>% 
  summarize(FREH = sum(FREH_3), .groups = "drop")

GH_nbhd_map <- 
  GH %>% 
  filter(status != "B", (date == "2019-05-01" | date == "2021-05-01")) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(nbhd) %>% 
  st_drop_geometry() %>% 
  group_by(nbhd, year = year(date)) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE), .groups = "drop") %>% 
  as_tibble()

housing_loss_nbhd_map <- 
  nbhd %>% 
  left_join(FREH_nbhd_map) %>% 
  left_join(GH_nbhd_map) %>% 
  mutate(GH = replace_na(GH, 0L),
         housing_loss_pct = (FREH + GH) / dwellings) |> 
  filter(!is.na(housing_loss_pct))

figure_8 <- 
  housing_loss_nbhd_map |> 
  ggplot() +
  geom_sf(data = state, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = housing_loss_pct), size = 0.3, colour = "white") +
  geom_sf(data = water, fill = "white", colour = "transparent") +
  scale_fill_stepsn(colours = col_palette[c(3, 3, 4, 1, 2, 2)], 
                    na.value = "grey80", limits = c(0, 0.02), 
                    oob = scales::squish, labels = scales::label_percent(0.1)) +
  guides(fill = guide_colourbar(title = "% housing\nlost to STR", 
                                title.vjust = 1)) + 
  facet_wrap(vars(year), nrow = 1) +
  gg_bbox(housing_loss_nbhd_map) +
  theme_void() +
  theme(text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(family = "Futura", size = 5),
        panel.border = element_rect(colour = "white", size = 2),
        legend.position = "right")

ggsave("output/figures/figure_8.pdf", plot = figure_8, width = 8, 
       height = 5.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_8.pdf")


# Figure 9 Housing loss share of listings ---------------------------------

housing_loss_share <- 
  daily |> 
  filter(housing, status != "B", listing_type == "Entire home/apt") |> 
  group_by(date) |> 
  summarize(pct = mean(FREH_3[listing_type == "Entire home/apt"] > 0.5)) |> 
  mutate(pct = slide_dbl(pct, mean, .before = 13)) |> 
  filter(date >= "2016-10-01")

figure_9 <- 
  housing_loss_share %>% 
  ggplot(aes(date, pct)) +
  geom_vline(aes(xintercept = as.Date("2019-11-01")), linetype = 3) +
  geom_line(colour = col_palette[5], lwd = 1) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_x_date(name = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", 
                            face = "plain"),
        legend.title = element_text(family = "Futura", 
                                    face = "bold"),
        legend.text = element_text(family = "Futura")
  )

ggsave("output/figures/figure_9.pdf", plot = figure_9, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_9.pdf")


# Figure 10 Host revenue distribution -------------------------------------

revenue_colour <- colorRampPalette(col_palette[c(1, 4, 2, 3, 5)])(10)

host_deciles <-
  daily %>%
  filter(housing, year(date) == 2020, status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>% 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) %>% 
  select(-all) %>% 
  pivot_longer(everything(), names_to = "percentile", values_to = "value") %>% 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) %>% 
  mutate(perfect_distribution = 0.1,
         decile = 1:10,
         dummy_1 = perfect_distribution,
         dummy_2 = value) %>%  
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) %>%
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") %>% 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) %>% 
  group_by(position) %>% 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) %>% 
  ungroup() %>% 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

figure_10 <- 
  host_deciles %>% 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  geom_text(aes(x = 0.02, y = absolute_val, label = display_percentile), 
            data = filter(host_deciles, position == 0, decile <= 2),
            family = "Futura", hjust = 0) +
  geom_text(aes(x = 0.98, y = absolute_val, label = display_val), 
            data = filter(host_deciles, position == 1, decile <= 2),
            family = "Futura", hjust = 1) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

ggsave("output/figures/figure_10.pdf", plot = figure_10, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_10.pdf")
