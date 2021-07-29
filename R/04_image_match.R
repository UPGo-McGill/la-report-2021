#### 04 IMAGE MATCH ############################################################

source("R/01_startup.R")
library(matchr)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
rm(daily, host)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data 2/Scrape photos/los_angeles/ab"
if (!dir.exists(dl_location)) dir.create(dl_location, recursive = TRUE)


# Download photos ---------------------------------------------------------

property |> 
  transmute(photos = coalesce(ab_image_url, ha_image_url),
            id = property_ID) |> 
  download_images(dl_location)


# Get signatures ----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location), full.names = TRUE)
ab_sigs <- create_signature(ab_paths)

ab_sigs <- create_signature(ab_paths[-c(12190, 76698)])
qsave(ab_sigs, file = "output/ab_sigs.qs", nthreads = availableCores())

rm(dl_location, ab_paths)


# Match images ------------------------------------------------------------

ab_matrix <- match_signatures(ab_sigs)
ab_matches <- identify_matches(ab_matrix)
rm(ab_matrix)
ab_changes <- compare_images(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)


# Save output -------------------------------------------------------------

qsave(ab_matches, file = "output/matches_raw.qs", nthreads = availableCores())
qsave(ab_changes, file = "output/match_changes.qs", nthreads = availableCores())
