#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(tidycensus)
library(osmdata)



# State, county & city boundaries -----------------------------------------

water <- map_dfr(c("111", "037", "029", "071", "059", "065"), 
                 ~tigris::area_water(state = "CA", county = .x)) |> 
  st_transform(32611) |> 
  st_union()

state <- 
  tigris::states() |> 
  filter(STUSPS == "CA") |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(geometry) |> 
  st_transform(32611)

county <- 
  tigris::counties("CA") |> 
  filter(NAME == "Los Angeles") |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(id = COUNTYFP, name = NAME, geometry) |> 
  st_transform(32611) |> 
  st_set_agr("constant") |> 
  st_cast("POLYGON") |> 
  filter(st_area(geometry) == max(st_area(geometry)))
  
city <- 
  tigris::places("CA") |> 
  filter(NAME == "Los Angeles") |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(geometry) |> 
  st_transform(32611)


# CTs ---------------------------------------------------------------------

CT <- 
  get_acs(geography = "tract", 
          variables = c("B25003_001", "B25003_003", "B25064_001", "B25065_001"), 
          state = "CA", geometry = TRUE, cache_table = TRUE) |>
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32611) |> 
  st_set_agr("constant")

CT_geom <- 
  CT |> 
  filter(variable == "B25003_001") |> 
  select(GEOID, geometry)

CT <- 
  CT |> 
  select(-moe) |> 
  st_drop_geometry() |> 
  pivot_wider(names_from = variable, values_from = estimate) |> 
  rename(dwellings = B25003_001, renter = B25003_003, median_rent = B25064_001,
         agg_rent = B25065_001) |> 
  left_join(CT_geom, by = "GEOID") |> 
  st_as_sf() |> 
  st_set_agr("constant")
  
CT_city <- 
  CT |> 
  st_centroid() |> 
  st_filter(city) |> 
  st_drop_geometry() |> 
  select(GEOID)

CT <- 
  CT |> 
  filter(GEOID %in% CT_city$GEOID)
  
rm(CT_geom, CT_city)


# Neighbourhoods ----------------------------------------------------------

nbhd <- 
  read_sf("data/LA_nbhd/geo_export_111f163d-31cc-4608-a257-aa7eaab5c247.shp") |> 
  st_transform(32611) |> 
  st_set_agr("constant") |> 
  filter(type == "segment-of-a-city") |> 
  select(nbhd = name)

housing <- 
  read_sf("data/housing_units.csv") |> 
  filter(Year == 2019) |> 
  mutate(Count = as.numeric(Count)) |> 
  group_by(neighborhood) |> 
  summarize(dwellings = sum(Count))

nbhd <- 
  nbhd |> 
  inner_join(housing, by = c("nbhd" = "neighborhood")) |> 
  relocate(dwellings, .after = nbhd)

rm(housing)


# Streets -----------------------------------------------------------------

streets <- 
  getbb("Los Angeles") |> 
  opq(timeout = 200) |> 
  add_osm_feature(key = "highway", value = "primary") |> 
  add_osm_feature(key = "highway", value = "secondary") |> 
  osmdata_sf()

streets <-
  rbind(
    streets$osm_polygons |> st_set_agr("constant") |> st_cast("LINESTRING"), 
    streets$osm_lines) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32611) |> 
  st_set_agr("constant") |> 
  select(osm_id, name, highway, geometry)


# Save output -------------------------------------------------------------

qsavem(city, county, CT, nbhd, state, streets, water, 
       file = "output/geometry.qsm", nthreads = availableCores())
