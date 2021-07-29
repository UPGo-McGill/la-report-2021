#### 10 SUPPLEMENTAL ANALYSIS ##################################################

source("R/01_startup.R")
plan(multisession, workers = 10)


# Load previous data ------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())


# Get other bookings ------------------------------------------------------

upgo_connect()

other_bookings <-
  daily_remote |> 
  filter(country == "United States", city %in% c("New York", "San Francisco"), 
         status == "R", housing, start_date >= "2019-01-01") |> 
  collect()

upgo_disconnect()

other_bookings <- 
  other_bookings |> 
  strr_expand()

# Add minimum stays
set.seed(2021)
min_stay_remote <- tbl(.con, "min_stay")

min_stay <- 
  min_stay_remote %>% 
  filter(property_ID %in% !!unique(other_bookings$property_ID)) %>% 
  collect()

min_stay <- 
  min_stay %>% 
  arrange(property_ID, start_date) %>% 
  group_by(property_ID) %>% 
  mutate(change_date = as.Date(slider::slide_int(
    start_date, ~{
      x <- (.x[1] + 1):.x[2]
      x[sample.int(length(x), 1, 
                   prob = ((.x[1] + 1):.x[2] - as.integer(.x[1])) ^ 0.5)]
    }, 
    .before = 1, .complete = TRUE), origin = "1970-01-01")) %>% 
  ungroup() %>% 
  mutate(start_date = coalesce(change_date, start_date)) %>% 
  select(-change_date)

other_bookings <- 
  other_bookings %>% 
  left_join(min_stay, by = c("property_ID", "date" = "start_date"))

# Get first missing date
min_missing <- 
  other_bookings %>% 
  filter(is.na(minimum_stay)) %>% 
  group_by(property_ID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(property_ID, date)

min_fill <- 
  min_stay %>% 
  filter(property_ID %in% min_missing$property_ID) %>% 
  left_join(min_missing, by = "property_ID") %>% 
  group_by(property_ID) %>% 
  filter(start_date < date) %>% 
  filter(start_date == max(start_date)) %>% 
  ungroup() %>% 
  select(property_ID, date, new_min = minimum_stay)

min_extra <- 
  min_missing %>% 
  filter(!property_ID %in% min_fill$property_ID)

min_extra <- 
  min_stay %>% 
  filter(property_ID %in% min_extra$property_ID) %>% 
  group_by(property_ID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(property_ID, new_min = minimum_stay) %>% 
  left_join(min_extra, ., by = "property_ID")

min_fill <- 
  bind_rows(min_fill, min_extra) %>% 
  arrange(property_ID)

other_bookings <- 
  other_bookings %>% 
  left_join(min_fill, by = c("property_ID", "date")) %>% 
  mutate(minimum_stay = coalesce(new_min, minimum_stay)) %>% 
  select(-new_min)

other_bookings <- 
  other_bookings %>% 
  arrange(property_ID, date) %>% 
  fill(minimum_stay, .direction = "down")

rm(min_extra, min_fill, min_missing, min_stay, min_stay_remote)

other_bookings <- 
  other_bookings |> 
  filter(minimum_stay <= 29)

qsave(other_bookings, file = "output/other_bookings.qs",
      nthreads = availableCores())


# Other platforms ---------------------------------------------------------

# Get reg-necessary Airbnb/Vrbo STRs
num_reg_str <-
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", minimum_stay < 30) |> 
  group_by(venice = !is.na(nbhd) & nbhd == "Venice") |> 
  summarize(ab = sum(!is.na(ab_property)),
            vb = sum(!is.na(ha_property)))

# Get reg-necessary STRs with scrape
num_scraped_str <-
  property |> 
  st_drop_geometry() |> 
  filter(!is.na(reg_status), reg_status != "No listing") |> 
  filter(minimum_stay < 30) |> 
  group_by(venice = !is.na(nbhd) & nbhd == "Venice") |> 
  summarize(ab = sum(!is.na(ab_property)),
            vb = sum(!is.na(ha_property)))

# Get non-cross-listed Vrbos
num_unique_vb <- 
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", minimum_stay < 30, 
         str_starts(property_ID, "ha-")) |> 
  nrow()

# Manually input listing counts
all_platforms <- 
  tibble(
    platform = c("Airbnb", "HomeAway/VRBO", "booking.com", "FlipKey", 
                 "misterb&b", "PerfectPlaces", "kid&coe", "atraveo", "tripz",
                 "BudandBreakfast", "HomeEscape"),
    LA = c(sum(num_reg_str$ab), sum(num_reg_str$vb), 412, 1024, 301, 9, 30, 2, 
           76, 33, 17),
    ven = c(num_reg_str$ab[2], num_reg_str$vb[2], 42, 31, 3, 2, 5, 0, 7, 0, 1))

# Calculate overlap with AB in registration numbers
ab_overlap <- map_dfr(all_platforms$platform, ~{
  reg_city |> 
    filter(str_detect(platform, .x)) |> 
    count(ab_vb = str_detect(platform, "Airbnb")) |> 
    mutate(platform = .x, .before = ab_vb) |> 
    pivot_wider(names_from = ab_vb, values_from = n)
    }) |> 
  set_names(c("platform", "overlap", "no_overlap")) |> 
  replace_na(list(overlap = 0, no_overlap = 0))

# Use AB/VB overlap from property file to establish regulation overlap baseline
vrbo_listing_overlap <- 
  property |> 
  st_drop_geometry() |> 
  filter(scraped >= "2021-06-20", minimum_stay < 30) |> 
  mutate(ab_vb = case_when(
    !is.na(ab_property) & !is.na(ha_property) ~ "both",
    !is.na(ab_property) ~ "ab",
    !is.na(ha_property) ~ "vb"
  )) |> 
  count(ab_vb) |> 
  summarize(pct = n[ab_vb == "both"] / (sum(n[ab_vb %in% c("both", "vb")]))) |> 
  pull(pct)

vrbo_reg_overlap <- 
  ab_overlap |> 
  filter(platform == "HomeAway/VRBO") |> 
  summarize(pct = overlap / (overlap + no_overlap)) |> 
  pull(pct)

# Add overlap to all_platforms
all_platforms <-
  all_platforms |> 
  left_join(ab_overlap) |> 
  mutate(new_list = LA * no_overlap / (overlap + no_overlap)) |> 
  # Rescale to match known Vrbo STR proportion
  mutate(new_list = new_list * num_unique_vb / 
           new_list[platform == "HomeAway/VRBO"],
         new_list = round(new_list)) |> 
  mutate(new_list = if_else(platform == "Airbnb", LA, new_list))

# Save output
qsave(all_platforms, "output/all_platforms.qs")
