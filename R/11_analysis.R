#### ANALYSIS ##################################################################

source("R/01_startup.R")
plan(multisession, workers = 10)

property <- qread("output/property.qs", nthreads = availableCores())
daily <- qread("output/daily.qs", nthreads = availableCores())
daily_all <- qread("output/daily_all.qs", nthreads = availableCores())
GH <- qread("output/GH.qs", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
all_platforms <- qread("output/all_platforms.qs")


# Prepare new objects -----------------------------------------------------

# 2019 active
active_2019 <-
  daily %>%
  filter(housing, status %in% c("R", "A"), year(date) == 2019) %>%
  pull(property_ID) %>%
  unique()

# 2020 active
active_2020 <- 
  daily %>%
  filter(housing, status %in% c("R", "A"), year(date) == 2020) %>%
  pull(property_ID) %>% 
  unique()

# 2021 active
active_2021 <- 
  daily %>%
  filter(housing, status %in% c("R", "A"), year(date) == 2021) %>%
  pull(property_ID) %>% 
  unique()

# 2019 revenue
revenue_2019 <-
  daily %>%
  filter(housing, status == "R", year(date) == 2019) |>
  summarize(revenue = sum(price)) |> 
  pull(revenue)

# 2020 revenue
revenue_2020 <-
  daily %>%
  filter(housing, status == "R", year(date) == 2020) |>
  summarize(revenue = sum(price)) |> 
  pull(revenue)

# 2021 revenue
revenue_2021 <-
  daily %>%
  filter(housing, status == "R", year(date) == 2021) |>
  summarize(revenue = sum(price)) |> 
  pull(revenue)

# LTM revenue
revenue_LTM <-
  daily %>%
  filter(housing, status == "R", date >= "2020-06-01") |>
  summarize(revenue = sum(price)) |> 
  pull(revenue)


# Active daily listings ---------------------------------------------------

active_blocked_table <- 
  daily_all %>% 
  filter(housing, year(date) >= 2019) %>% 
  count(date, min30 = minimum_stay >= 30, B = status == "B", 
        year = year(date)) %>% 
  group_by(year, min30, B) %>% 
  summarize(listings = mean(n), .groups = "drop")

hosts_table <- 
  daily %>% 
  filter(housing, year(date) >= 2019) %>%
  count(date, host_ID, B = status == "B", year = year(date)) %>% 
  count(date, B, year) %>% 
  group_by(year, B) %>% 
  summarize(hosts = mean(n), .groups = "drop")

str_ltr_rev_table <- 
  daily_all |> 
  filter(housing, date >= "2019-01-01", status == "R") |> 
  group_by(year = year(date), min30 = minimum_stay >= 30) |> 
  summarize(revenue = sum(price), .groups = "drop")

# Average active and blocked STR/LTR daily listings in 2019-2021
active_blocked_table |> 
  mutate(listings = scales::comma(listings, 10))

# Average number of hosts (taking out blocked 365 days)
hosts_table |> 
  mutate(hosts = scales::comma(hosts, 10))

# Total annual revenue, 2019 and LTM
scales::dollar(revenue_2019, 100000)
scales::dollar(revenue_LTM, 100000)

# Average revenue per active listing, 2019
(revenue_2019 / active_blocked_table$listings[1]) |> 
  scales::dollar(100)

# Average revenue per active host, 2019
(revenue_2019 / hosts_table$hosts[1]) |> 
  scales::dollar(100)

# Average revenue per active listing, 2021
(revenue_2021 / active_blocked_table$listings[9]) |> 
  scales::dollar(100)

# Average revenue per active host, 2021
(revenue_2021 / hosts_table$hosts[5]) |> 
  scales::dollar(100)

# Average revenue per all listings, 2021
(revenue_2021 / sum(active_blocked_table$listings[9:10])) |> 
  scales::dollar(100)

# Average revenue per all hosts, 2021
(revenue_2021 / sum(hosts_table$hosts[5:6])) |> 
  scales::dollar(100)

# STR versus LTR revenue
str_ltr_rev_table |> 
  filter(year == 2021) |> 
  mutate(revenue = scales::dollar(revenue, 100000))

# Date and amount of highest activity
daily %>% 
  filter(housing, status != "B") %>% 
  count(date, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(n = scales::comma(n, 10))


# Location of STR listings and revenue ------------------------------------

nbhd_breakdown <- 
  daily |>  
  filter(housing, !is.na(nbhd), status != "B", year(date) >= 2019) |> 
  group_by(date, nbhd) |> 
  summarize(n = n(), revenue = sum(price[status == "R"]), .groups = "drop") |> 
  left_join(st_drop_geometry(nbhd)) |> 
  group_by(nbhd, dwellings) |> 
  summarize(active_2019 = max(0, mean(n[date <= "2019-12-31"]), na.rm = TRUE),
            active_2021 = max(0, mean(n[date >= "2021-01-01"]), na.rm = TRUE),
            rev_ltm = max(0, sum(revenue[date >= "2020-06-01"]), na.rm = TRUE),
            .groups = "drop") |> 
  mutate(active_dwelling_2019 = active_2019 / dwellings,
         active_dwelling_2021 = active_2021 / dwellings,
         listings_pct = active_2021 / sum(active_2021, na.rm = TRUE),
         rev_pct = rev_ltm / sum(rev_ltm, na.rm = TRUE)) |> 
  select(nbhd, active_2019, active_2021, active_dwelling_2019, 
         active_dwelling_2021, rev_ltm, listings_pct, rev_pct)

# Figures for top neighbourhoods
nbhd_breakdown %>% 
  filter(nbhd %in% c("Venice", "Hollywood", "Downtown", "Hollywood Hills West"))

# Table 2.2
library(gt)
nbhd_breakdown |> 
  select(-listings_pct, -rev_pct) |> 
  filter(active_2021 > 60) |> 
  arrange(desc(active_2021)) |> 
  mutate(active_2019 = scales::comma(active_2019, 10),
         active_2021 = scales::comma(active_2021, 10),
         active_dwelling_2019 = scales::percent(active_dwelling_2019, 0.1),
         active_dwelling_2021 = scales::percent(active_dwelling_2021, 0.1),
         rev_ltm = round(rev_ltm),
         rev_ltm = paste0("$", str_sub(rev_ltm, 1, -7), ".",
                  str_sub(rev_ltm, -6, -6), " million"))  |> 
  set_names(c("Neighborhood",
              "Active listings (2019)",
              "Active listings (2021)",
              "Active listings as % of dwellings (2019)",
              "Active listings as % of dwellings (2021)",
              "Revenue (last 12 months)")) |> 
  gt() |> 
  tab_header(
    title = "Neighborhood breakdown",
    subtitle = "Neighborhoods with at least 200 daily active listings, 2021"
  ) |> 
  opt_row_striping()

# Need to add LA row, with % listings per dwelling
daily |> 
  filter(housing, status != "B", year(date) %in% c(2019, 2021)) |> 
  count(date) |> 
  group_by(year = year(date)) |> 
  summarize(active_listings = round(mean(n), digit = -1)) |> 
  mutate(active_pct = active_listings / sum(nbhd$dwellings),
         active_pct = scales::percent(active_pct, 0.1))


# Multilistings -----------------------------------------------------------

# ML listings
daily |>
  filter(housing, status != "B", year(date) %in% c(2019, 2020)) |> 
  group_by(year = year(date)) |> 
  count(multi) |> 
  summarize(multi_listings = n[2] / sum(n)) |> 
  mutate(multi_listings = scales::percent(multi_listings, 0.1))

# 2020 ML revenue
daily |> 
  filter(housing, status == "R", year(date) %in% c(2019, 2020)) |> 
  group_by(year = year(date), multi) |> 
  tally(price) |> 
  summarize(multi_rev = n[2] / sum(n)) |> 
  mutate(multi_rev = scales::percent(multi_rev, 0.1))

#' [3] Dec 2020 ML revenue
daily %>% 
  filter(housing, status == "R", year(date) == 2020, month(date) == 12) %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n)) %>% 
  pull(multi_rev) %>% 
  scales::percent(0.1)


# Comparison with NYC/SF --------------------------------------------------

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

# 2019 difference
bookings_comparison |> 
  pivot_longer(c(R_june, R_feb), names_to = "index") |> 
  filter(year >= 2020 | index == "R_june") |> 
  filter(index == "R_june", date >= "2019-09-01", date <= "2020-02-01") |> 
  group_by(city) |> 
  summarize(change = mean(value) - 100)

# 2020 difference
bookings_comparison |> 
  pivot_longer(c(R_june, R_feb), names_to = "index") |> 
  filter(year >= 2020 | index == "R_june") |> 
  filter(date >= "2012-03-01") |> 
  group_by(index, city) |> 
  summarize(change = mean(value) - 100)



# Legal and illegal listings ----------------------------------------------

# All displayed listings in June
property |> 
  filter(str_starts(property_ID, "ab"), housing, scraped >= "2021-06-20") |> 
  nrow()

# All with 30-day minimums
property |> 
  filter(str_starts(property_ID, "ab"), housing, scraped >= "2021-06-20") |> 
  filter(minimum_stay == 30) |> 
  nrow()

# 30-day min with reg
property |> 
  st_drop_geometry() |> 
  filter(str_starts(property_ID, "ab"), housing, scraped >= "2021-06-20") |> 
  filter(minimum_stay == 30) |> 
  count(reg_status) |> 
  mutate(pct = n / sum(n))

# 31-day min
property |> 
  filter(str_starts(property_ID, "ab"), housing, scraped >= "2021-06-20") |> 
  filter(minimum_stay >= 31) |> 
  nrow()

# All listings
all_platforms$new_list |> 
  sum() |> 
  scales::comma(10)

# On Airbnb/Vrbo
all_platforms |> 
  group_by(ab_vb = platform %in% c("Airbnb", "HomeAway/VRBO")) |> 
  summarize(listings = sum(new_list)) |> 
  mutate(listing_pct = scales::percent(listings / sum(listings), 0.1)) |> 
  mutate(listings = scales::comma(listings, 10))

# Missing reg but close to border
border_prop <- 
  property |> 
  filter(reg_status == "Missing") |> 
  filter(scraped >= "2021-06-20", str_starts(property_ID, "ab-"),
         minimum_stay < 30) |> 
  select(property_ID, geometry) |> 
  mutate(geometry = st_buffer(st_centroid(geometry), 200)) |> 
  mutate(at_border = st_intersects(geometry, city_boundary)) |> 
  mutate(at_border = as.logical(lengths(at_border))) |> 
  pull(at_border) |> 
  mean()

# Registration proportion
reg_props <-
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", str_starts(property_ID, "ab-"),
         minimum_stay < 30) |> 
  count(reg_status) |> 
  filter(reg_status != "No listing") |> 
  summarize(missing = n[reg_status == "Missing"] * (1 - border_prop),
            missing_pct = missing / sum(n),
            missing_at_border = n[reg_status == "Missing"] * border_prop,
            missing_at_border_pct = missing_at_border / sum(n),
            exempt = n[reg_status == "Exempt"],
            exempt_pct = exempt / sum(n),
            number = sum(n[reg_status %in% c("Fake", "Invalid", "Valid")]),
            number_pct = number / sum(n))

# Unlicensed %
round(reg_props$missing_pct * sum(all_platforms$new_list))

# Actual scrape candidates to check
all_candidates <- 
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", str_starts(property_ID, "ab-"),
         minimum_stay < 30) |> 
  filter(reg_status %in% c("Valid", "Invalid", "Fake"))

# Duplicates
which_dup <- 
  all_candidates |> 
  select(property_ID, host_ID, listing_type, reg_status, reg_number) |> 
  filter(reg_status %in% c("Valid", "Invalid")) |> 
  group_by(reg_number) |> 
  filter(n() > 1) |> 
  filter(mean(listing_type == "Private room") < 1) |> 
  ungroup()

n_dup <- nrow(which_dup)
  
# Fake
n_fake <- 
  all_candidates |> 
  filter(reg_status == "Fake") |> 
  nrow()

# Duplicate or fake
n_dup_or_fake <- n_dup + n_fake
  
# Duplicate or fake %
pct_dup_or_fake <- n_dup_or_fake / nrow(all_candidates)

# ML and 183 nights
candidates <- 
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", str_starts(property_ID, "ab-"),
         minimum_stay < 30) |> 
  filter(reg_status %in% c("Valid", "Invalid")) |> 
  filter(!property_ID %in% which_dup$property_ID)

# 183 nights
pid_183 <- 
  daily |> 
  filter(property_ID %in% candidates$property_ID) |> 
  filter(status == "R", year(date) >= 2020) |> 
  count(year = year(date), property_ID) |> 
  ungroup() |> 
  filter(n >= 183) |> 
  pull(property_ID) |> 
  unique()

length(pid_183) / nrow(all_candidates)

# Conservative ML
pid_ML <- 
  daily |> 
  filter(property_ID %in% candidates$property_ID) |> 
  filter(!property_ID %in% pid_183) |> 
  filter(multi) |> 
  count(property_ID, host_ID) |> 
  group_by(host_ID) |> 
  slice(2:max(n(), 2)) |> 
  ungroup() |> 
  pull(property_ID) |> 
  unique()

# Aggressive ML
pid_ML_aggressive <- 
  daily |> 
  filter(property_ID %in% candidates$property_ID) |> 
  filter(!property_ID %in% pid_183) |> 
  filter(multi) |> 
  count(property_ID) |> 
  pull(property_ID) |> 
  unique()

length(pid_ML_aggressive) / nrow(all_candidates)
length(pid_ML) / nrow(all_candidates)
pct_183_or_ML <- (length(pid_183) + length(pid_ML)) / nrow(all_candidates)

# Total illegal % among licensed listings
pct_dup_or_fake + pct_183_or_ML

# % of all listings not missing/exempt
valid_pct <- 1 - reg_props$missing_pct - reg_props$missing_at_border_pct - 
  reg_props$exempt_pct

# Final numbers
final_reg <- 
  tibble(
    category = c("missing", "missing_at_border", "exempt", "dup_or_fake", 
                 "120_or_ML", "legal"),
    pct = c(reg_props$missing_pct, reg_props$missing_at_border_pct, 
            reg_props$exempt_pct, pct_dup_or_fake * valid_pct, 
            pct_183_or_ML * valid_pct, 
            (1 - pct_dup_or_fake - pct_183_or_ML) * valid_pct)) |> 
  mutate(listings = sum(all_platforms$new_list) * pct)

# Number and pct illegal
final_reg |> 
  filter(!category %in% c("exempt", "missing_at_border", "legal")) |> 
  summarize(across(c(pct, listings), sum))


# Housing loss ------------------------------------------------------------

FREH <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", day(date) == 1) %>% 
  group_by(date) %>% 
  summarize(across(c(FREH_3), sum))

FREH_nbhd <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", month(date) == 12, day(date) == 1) %>% 
  group_by(nbhd, date) %>% 
  summarize(FREH = sum(FREH_3), .groups = "drop") %>% 
  mutate(date = year(date))

GH_nbhd <- 
  GH %>% 
  filter(date >= "2016-01-01", month(date) == 12, day(date) == 31) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(nbhd) %>% 
  st_drop_geometry() %>% 
  group_by(nbhd, date) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE), .groups = "drop") %>% 
  mutate(date = year(date))

# FREH in 2019-09
FREH %>% 
  filter(date == "2019-09-01") %>% 
  pull(FREH_3) %>%
  scales::comma(10)

# GH in 2019-09
GH %>% 
  filter(date == "2019-09-30", status != "B") %>% 
  pull(housing_units) %>% sum() %>% 
  scales::comma(10)

# FREH in 2020-01
FREH %>% 
  filter(date == "2020-01-01") %>% 
  pull(FREH_3) %>%
  scales::comma(10)

# GH in 2020-01
GH %>% 
  filter(date == "2020-01-31", status != "B") %>% 
  pull(housing_units) %>% sum() %>% 
  scales::comma(10)

#' [1] FREH in 2021-03
FREH %>% 
  filter(date == "2021-03-01") %>% 
  pull(FREH_3) %>%
  scales::comma(10)

#' [2] GH in 2021-03
GH %>% 
  filter(date == "2021-03-31", status != "B") %>% 
  pull(housing_units) %>% sum() %>% 
  scales::comma(10)

#' [1] FREH in 2021-05
FREH %>% 
  filter(date == "2021-05-01") %>% 
  pull(FREH_3) %>%
  scales::comma(10)

#' [2] GH in 2021-05
GH %>% 
  filter(date == "2021-05-31", status != "B") %>% 
  pull(housing_units) %>% sum() %>% 
  scales::comma(10)

housing_loss_nbhd_map |> 
  filter(nbhd == "Venice")

# Housing loss table
housing_loss_nbhd_map |> 
  st_drop_geometry() |> 
  mutate(housing_loss = FREH + GH, .before = housing_loss_pct) |> 
  select(-dwellings, -FREH, -GH) |> 
  pivot_wider(names_from = year, values_from = c(housing_loss, housing_loss_pct)) |> 
  arrange(-housing_loss_2021) |> 
  filter(housing_loss_2021 >= 55) |> 
  mutate(across(c(housing_loss_2019, housing_loss_2021), 
                scales::comma, accuracy = 10),
         across(c(housing_loss_pct_2019, housing_loss_pct_2021), 
                scales::percent, accuracy = 0.1)) |> 
  gt()

housing_loss_nbhd_map |> 
  st_drop_geometry() |> 
  group_by(year) |> 
  summarize(housing_loss = sum(FREH, GH),
            housing_loss_pct = housing_loss / sum(dwellings))


# Revenue distribution ----------------------------------------------------

host_rev <-
  daily %>%
  filter(housing, year(date) == 2020, status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price))

# Max host listings
host_rev %>% 
  filter(rev == max(rev)) %>% 
  pull(host_ID) %>% 
  {filter(property, host_ID %in% .)} %>% 
  filter(housing) %>% 
  filter(year(scraped) == 2020 | 
           (year(scraped) == 2021 & year(created) < 2021)) %>% 
  nrow() %>% 
  scales::comma(10)

#' [2] Max host rev
max(host_rev$rev) %>% scales::dollar(100000)

#' [1] Median host revenue
median(host_rev$rev) %>% scales::dollar(100)

#' [2] Top earner
max(host_rev$rev) %>% scales::dollar(100000)

#' [3] Hosts above $500,000
host_rev %>% 
  filter(rev >= 500000) %>% 
  nrow()

#' [4] Hosts above $500,000 in 2019
daily %>% 
  filter(housing, status == "R", year(date) == 2019) %>% 
  group_by(host_ID) %>% 
  summarize(rev = sum(price)) %>% 
  filter(rev > 500000) %>% 
  nrow()

#' [5] Total annual revenue
revenue_2020 %>% 
  scales::dollar(100000)

#' [6] Top 10% revenue
host_rev %>% 
  summarize(top_10 = sum(rev[rev > quantile(rev, .9)]) / sum(rev)) %>% 
  pull(top_10) %>% 
  scales::percent(0.1)

#' [7] Top 5% revenue
host_rev %>% 
  summarize(top_5 = sum(rev[rev > quantile(rev, .95)]) / sum(rev)) %>% 
  pull(top_5) %>% 
  scales::percent(0.1)

#' [8] Top 1% revenue
host_rev %>% 
  summarize(top_1 = sum(rev[rev > quantile(rev, .99)]) / sum(rev)) %>% 
  pull(top_1) %>% 
  scales::percent(0.1)

#' Table 2.5
host_rev %>% 
  pull(rev) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2) %>% 
  drop_na() %>% 
  gt() %>% 
  tab_header(title = "Host income") %>%
  opt_row_striping() 


