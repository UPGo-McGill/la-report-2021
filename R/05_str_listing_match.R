#### 08 STR LISTING MATCH ######################################################

source("R/01_startup.R")
library(matchr)

# Load previous data ------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())
ab_matches <- qread("output/matches_raw.qs", nthreads = availableCores())
dl_location <- "/Volumes/Data 2/Scrape photos/los_angeles/ab"


# Clean up ab_matches -----------------------------------------------------

ab_matches <-
  ab_matches %>%
  filter(match == "match") %>%
  mutate(
    x_name = str_replace_all(get_path(x_sig), paste0(dl_location, "/|-1.jpg"), ""),
    y_name = str_replace_all(get_path(y_sig), paste0(dl_location, "/|-1.jpg"), "")
  ) %>%
  select(x_name, y_name) %>% 
  filter(x_name %in% property$property_ID, y_name %in% property$property_ID)

rm(dl_location)


# Filter matches to < 500 m distance --------------------------------------

matches_join <- 
  property %>% 
  select(x_name = property_ID, geometry) %>% 
  inner_join(ab_matches, by = "x_name")

y_to_join <- 
  property %>% 
  as_tibble() %>% 
  select(y_name = property_ID, y_geom = geometry)

ab_matches <- 
  matches_join %>% 
  inner_join(y_to_join, by = "y_name") %>% 
  relocate(y_name, .after = x_name) %>% 
  mutate(dist = map2_dbl(geometry, y_geom, st_distance), 
         .after = y_name) %>% 
  filter(dist < 500) %>% 
  st_drop_geometry() %>% 
  select(x_name, y_name)

rm(matches_join, y_to_join)


# Identify groupings ------------------------------------------------------

# Convert to list of pairs
pair_list <- pmap(ab_matches, c)

reduce <- function(x) {
  
  Reduce(function(a, b) {
    merge_index <- lapply(a, intersect, b)
    
    if (sum(lengths(merge_index)) > 0) {
      merge_index <- which(lengths(merge_index) > 0)
      merged <- a[merge_index]
      merged <- unlist(merged)
      merged <- union(merged, b)
      merged <- list(sort(merged))
      not_merged <- a[-merge_index]
      out <- c(merged, not_merged)
    } else out <- c(a, list(b))
  }, x, init = list())
  
}

# Merge lists
groupings <- reduce(pair_list)
rm(ab_matches, pair_list)


# Modify host_ID from groupings -------------------------------------------

host_IDs <-
  groupings %>%
  map(~{
    property %>%
      filter(property_ID %in% .x) %>%
      pull(host_ID) %>%
      unique()
  })

host_IDs <- reduce(host_IDs)
host_IDs <- map(host_IDs, sort)
host_IDs <- host_IDs[lengths(host_IDs) > 0]

host_change_table <-
  map_dfr(host_IDs, ~tibble(host_ID = .x, new_host = .x[[1]]))

property <-
  property %>%
  left_join(host_change_table) %>%
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = coalesce(new_host, host_ID)) %>%
  select(-new_host)

daily <-
  daily %>%
  left_join(host_change_table) %>%
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = if_else(is.na(new_host), host_ID, new_host)) %>%
  select(-new_host)

rm(host_change_table, host_IDs, reduce)


# Get matches -------------------------------------------------------------

# Get final activity date
property <-
  daily %>%
  filter(status != "B") %>%
  group_by(property_ID) %>%
  filter(date == max(date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(property_ID, active = date) %>%
  left_join(property, .) %>%
  select(property_ID:scraped, active, housing:GEOID, old_host, geometry)

group_matches <-
  groupings %>%
  map(~{
    property %>%
      filter(property_ID %in% .x, listing_type == "Entire home/apt") %>%
      mutate(active = if_else(is.na(active), created, active)) %>%
      filter(active >= created)
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 0]
group_matches <- group_matches %>% map(arrange, created)

group_matches <-
  group_matches %>%
  map(~{
    next_created <- c(.x$created, NA)
    # Drop the first element to shift all created dates up a row
    next_created <- next_created[2:length(next_created)]
    .x %>%
      mutate(next_created = next_created) %>%
      filter(active < next_created | is.na(next_created))
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 1]

rm(groupings)


# Collapse property_IDs ---------------------------------------------------

property_change_table <-
  map_dfr(group_matches,
          ~tibble(
            property_ID = .x$property_ID,
            new_PID =
              filter(.x, active - created == max(active - created)) %>%
              slice(1) %>%
              pull(property_ID),
            new_created = min(.x$created, na.rm = TRUE),
            new_scraped = max(.x$scraped, na.rm = TRUE),
            new_active = max(.x$active, na.rm = TRUE)))

daily <-
  daily %>%
  left_join(property_change_table) %>%
  mutate(old_PID = if_else(is.na(new_PID), NA_character_, property_ID),
         property_ID = if_else(is.na(new_PID), property_ID, new_PID)) %>%
  select(-new_PID, -new_created, -new_scraped, -new_active)

property_change_collapsed <-
  property_change_table %>%
  group_by(new_PID, new_created, new_scraped, new_active) %>%
  summarize(all_PIDs = list(property_ID)) |> 
  ungroup()

property_to_delete <-
  property_change_table %>%
  filter(property_ID != new_PID)

property <-
  property %>%
  left_join(property_change_collapsed, by = c("property_ID" = "new_PID")) %>%
  filter(!property_ID %in% property_to_delete$property_ID) %>%
  mutate(created = if_else(!is.na(new_created), new_created, created),
         scraped = if_else(!is.na(new_scraped), new_scraped, scraped),
         active = if_else(!is.na(new_active), new_active, active)) %>%
  select(-new_created, -new_scraped, -new_active) %>%
  select(-geometry, everything(), geometry)

rm(group_matches, property_change_collapsed, property_change_table,
   property_to_delete)


# Save output -------------------------------------------------------------

qsavem(property, daily, host, file = "output/str_processed.qsm",
       nthreads = availableCores())
