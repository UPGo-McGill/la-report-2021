#### 09 STR PROCESSING #########################################################

source("R/01_startup.R")
library(foreach)
library(data.table)
doParallel::registerDoParallel()


# Load previous data ------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())


# Recalculate active date -------------------------------------------------

daily_active <-
  daily %>%
  filter(status != "B") %>%
  group_by(property_ID) %>%
  filter(date == max(date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(property_ID, active_new = date)

property <-
  property %>%
  left_join(daily_active) %>%
  mutate(active = active_new) %>%
  select(-active_new)

daily <-
  daily %>%
  group_by(property_ID, date, status) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(property_ID, date) %>%
  filter(n() == 1 | (n() == 2 & status != "B")) %>%
  ungroup()

rm(daily_active)


# Add minimum stays -------------------------------------------------------

set.seed(2021)

min_stay_remote <- tbl(.con, "min_stay")

min_stay <- 
  min_stay_remote %>% 
  filter(property_ID %in% !!property$property_ID) %>% 
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

daily <- 
  daily %>% 
  left_join(min_stay, by = c("property_ID", "date" = "start_date")) %>% 
  relocate(minimum_stay, .after = old_PID)

# Get first missing date
min_missing <- 
  daily %>% 
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

daily <- 
  daily %>% 
  left_join(min_fill, by = c("property_ID", "date")) %>% 
  mutate(minimum_stay = coalesce(new_min, minimum_stay)) %>% 
  select(-new_min)

daily <- 
  daily %>% 
  arrange(property_ID, date) %>% 
  fill(minimum_stay, .direction = "down")

rm(min_extra, min_fill, min_missing, min_stay, min_stay_remote)


# Calculate multilistings -------------------------------------------------

# Get host IDs to merge
host_to_merge <- 
  property |> 
  filter(!is.na(old_host)) |> 
  st_drop_geometry() |> 
  select(new_host_ID = host_ID, host_ID = old_host)

# Merge them
host <- 
  host |> 
  left_join(host_to_merge, by = "host_ID") |> 
  mutate(host_ID = coalesce(new_host_ID, host_ID)) |> 
  group_by(host_ID, date, listing_type, housing) |> 
  summarize(count = sum(count, na.rm = TRUE), .groups = "drop")

# Calculate ML
daily <-
  daily |> 
  strr_multi(host) |> 
  as_tibble()

rm(host_to_merge)


# Interim save ------------------------------------------------------------

qsavem(property, daily, host, file = "output/str_processed.qsm",
       nthreads = availableCores())


# Calculate ghost hostels -------------------------------------------------

GH <-
  property %>%
  strr_ghost(start_date = "2015-01-01", end_date = max(daily$date))


# Interim save ------------------------------------------------------------

qsavem(property, daily, GH, file = "output/str_processed.qsm",
       nthreads = availableCores())


# Add daily status to GH --------------------------------------------------

daily_GH <-
  daily %>%
  filter(property_ID %in% unique(unlist(GH$property_IDs)))

setDT(daily_GH)

daily_GH <- daily_GH %>% select(property_ID:status)

status_fun <- function(x, y) {
  status <- unique(daily_GH[date == x & property_ID %in% y, status])
  fcase("R" %in% status, "R", "A" %in% status, "A", "B" %in% status, "B")
}

status <- foreach(i = 1:nrow(GH), .combine = "c") %dopar% {
    status_fun(GH$date[[i]], GH$property_IDs[[i]])
  }
  
GH$status <- status
GH <- GH %>% select(ghost_ID, date, status, host_ID:data, geometry)

rm(daily_GH, status_fun, status)


# Add GH status to daily --------------------------------------------------

GH_daily <-
  GH %>%
  st_drop_geometry() %>%
  select(date, property_IDs) %>%
  unnest(property_IDs) %>%
  mutate(GH = TRUE) %>%
  select(property_ID = property_IDs, date, GH)

daily <-
  daily %>%
  left_join(GH_daily, by = c("property_ID", "date")) %>%
  mutate(GH = if_else(is.na(GH), FALSE, GH))

rm(GH_daily)


# Split daily into daily and daily_all ------------------------------------

daily_all <- daily
daily <- daily_all %>% filter(minimum_stay <= 29)


# Save output -------------------------------------------------------------

qsave(property, file = "output/property.qs", nthreads = availableCores())
qsave(daily, file = "output/daily.qs", nthreads = availableCores())
qsave(daily_all, file = "output/daily_all.qs", nthreads = availableCores())
qsave(GH, file = "output/GH.qs", nthreads = availableCores())
