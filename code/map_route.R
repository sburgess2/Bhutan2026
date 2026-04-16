library(rnaturalearth)
library(rnaturalearthhires)
library(tidyverse)
library(exifr)
library(sf)
library(tigris) # Access geographic data from the US Census
library(tidygeocoder) # Automated geocoding
library(osrm)
library(ggrepel)
library(scales)
library(ggspatial)
library(leaflet)

bhutan_map <- ne_states(country = "Bhutan", returnclass = "sf")

clrs <- NatParksPalettes::natparks.pals("Yellowstone")

ggplot() +
  geom_sf(data = bhutan_map)



# Read all photos
# Code modified from https://www.r-bloggers.com/2016/11/extracting-exif-data-from-photos-using-r/

files <- list.files(
  path = "photos",
  pattern = "\\.HEIC$",
  full.names = TRUE
)

dat <- read_exif(files) |>
  select(
    SourceFile, DateTimeOriginal,
    GPSLongitude, GPSLatitude,
    GPSTimeStamp
  ) |>
  filter(!is.na(GPSLatitude))

write_csv(dat, "data/exifdata.csv")

# Make an interactive map
leaflet(dat) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(~GPSLongitude, ~GPSLatitude)

# Make a basic map

dat_geocode <- dat |>
  st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = st_crs("EPSG:4326"))

ggplot() +
  geom_sf(data = bhutan_map) +
  geom_sf(data = dat_geocode) +
  coord_sf(crs = st_crs("ESRI:102003"))
# Code adapted from https://www.andrewheiss.com/blog/2023/06/01/geocoding-routing-openstreetmap-r/

stops_address <- tribble(
  ~day, ~place, ~address,
  1, "Paro International Airport", "Paro International Airport, Paro, Bhutan",
  2, "Paro Dzong", "Rinpung, Paro, Bhutan",
  3, "Tiger's Nest", "Paro Taktsang, Paro, Bhutan",
  4, "Kila Goenpa Nunnery", "Kila Goenpa, Paro, Bhutan",
  4, "Chelela Pass", "Chele La Pass, Paro, Bhutan",
  4, "White Temple", "Lhakhang Karpo, Haa, Bhutan",
  5, "Tango Monastery", "Tango Monastery, Thimphu, Bhutan",
  5, "Thimphu", "Motithang Takin Preserve, Thimphu, Bhutan",
  6, "Thimphu", "Thimphu, Bhutan",
  6, "Thimphu", "Folk Heritage Museum Kawajangsa, Thimphu, Bhutan",
  7, "Dochula Pass", "Dochula Pass, Thimphu, Bhutan",
  7, "Teoprongchu", "Teoprongchu, Bhutan",
  7, "Chimi Lhakhang (The Temple of Fertility)", "Chimi Lhakhang, Punakha, Bhutan",
  8, "Khamsum Yulley Namgyal Chorten", "Khamsum Yulley Namgyal Chörten, Thimphu-Punakha Hwy, Bhutan",
  8, "Mo Chhu River, Punakha", "Khamsum Yueli, Namgyal 13001, Bhutan",
  8, "Sangchen Dorji Lhuendrup Nunnery", "Sangchhen Dorji Lhuendrup Nunnery, Punakha, Bhutan",
  8, "Wangdue Ecolodge", "Wangdue Ecolodge, Damina Village, Ngashigaykha, Rubesa, Wangdue Phodrang, 14001, Bhutan",
  9, "Wangdue Dzong", "Wangdue Phodrang Dzong, Wangdue Phodrang, Bhutan",
  9, "Lawala Pass", "Lawala Pass, Dungdungneysa, Bhutan",
  9, "Gangtey Monastery", "Gangtey Monastery, Phobjikha Valley, Bhutan",
  9, "Phobjikha View Point", "Phobjikha View Point, Phobjikha Valley, Bhutan",
  9, "Black Necked Crane Visitor Centre", "Black Necked Crane Visitor Centre, Phobjikha, Bhutan",
  9, "Gangtey Tent Resort", "Gangtey Tent Resort, Gangtey Phobjikha, Bhutan",
  10, "Kumbu", "Kumbu, Bhutan",
  10, "Kaychela Pass", "Kaychela Pass, Bhutan",
  10, "Longtey", "Longtey, Bhutan",
  10, "Gangtey Tent Resort", "Gangtey Tent Resort, Gangtey Phobjikha, Bhutan",
  11, "Himalayan Keys Forest Resort", "Himalayan Keys Forest Resort, Zhori Zur Lam, Thimphu, Bhutan",
  11, "TaBar Nye Monastery", "TaBar Nye, Thimphu, Bhutan",
  11, "Camp", "Thadrana Telecom Tower Junction, Thimphu, Bhutan",
  11, "Gyalpo Pelzang Peak", "Thadrana Telecom Tower point, Yusipang, Bhutan",
  12, "Hontsho", "Hontsho Picnic, Hungtsho, Bhutan",
  12, "Tashi Namgay Resort", "Tashi Namgay Resort, Paro, Bhutan",
  13, "Paro International Airport", "Paro International Airport, Paro, Bhutan"
)

stops_geocoded <- stops_address |>
  geocode(address, method = "osm") |>
  mutate(
    lat = case_when(
      place == "Kila Goenpa Nunnery" ~ 27.39064,
      place == "Khamsum Yulley Namgyal Chorten" ~ 27.62631,
      place == "Mo Chhu River, Punakha" ~ 27.62371,
      place == "Sangchen Dorji Lhuendrup Nunnery" ~ 27.54797,
      place == "Wangdue Ecolodge" ~ 27.47045,
      place == "Lawala Pass" ~ 27.51032,
      place == "Gangtey Monastery" ~ 27.47145,
      place == "Phobjikha View Point" ~ 27.47071,
      place == "Black Necked Crane Visitor Centre" ~ 27.46819,
      place == "Gangtey Tent Resort" ~ 27.47103,
      place == "Kumbu" ~ 27.50647,
      place == "Kaychela Pass" ~ 27.50639,
      place == "Longtey" ~ 27.5075,
      place == "Himalayan Keys Forest Resort" ~ 27.46988,
      place == "TaBar Nye Monastery" ~ 27.51009,
      place == "Camp" ~ 27.50897,
      place == "Gyalpo Pelzang Peak" ~ 27.50863,
      place == "Hontsho" ~ 27.50726,
      TRUE ~ lat
    ),
    long = case_when(
      place == "Kila Goenpa Nunnery" ~ 89.36146,
      place == "Khamsum Yulley Namgyal Chorten" ~ 89.80070,
      place == "Mo Chhu River, Punakha" ~ 89.80130,
      place == "Sangchen Dorji Lhuendrup Nunnery" ~ 89.84384,
      place == "Wangdue Ecolodge" ~ 89.89029,
      place == "Lawala Pass" ~ 90.15212,
      place == "Gangtey Monastery" ~ 90.15188,
      place == "Phobjikha View Point" ~ 90.20001,
      place == "Black Necked Crane Visitor Centre" ~ 90.15385,
      place == "Gangtey Tent Resort" ~ 90.15620,
      place == "Kumbu" ~ 90.19599,
      place == "Kaychela Pass" ~ 90.19788,
      place == "Longtey" ~ 90.1964,
      place == "Himalayan Keys Forest Resort" ~ 89.62807,
      place == "TaBar Nye Monastery" ~ 89.67089,
      place == "Camp" ~ 89.71630,
      place == "Gyalpo Pelzang Peak" ~ 89.71298,
      place == "Hontsho" ~ 89.71271,
      TRUE ~ long
    )
  )

stops_sf <- stops_geocoded |>
  st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))


write_csv(stops_geocoded, "data/stops_geocoded.csv")
all_stops_unique <- stops_geocoded |>
  distinct()

routes_raw <- stops_sf |>
  select(-address) |>
  rename(
    origin_geometry = geometry,
    origin_place = place
  ) |>
  mutate(
    destination_geometry = lead(origin_geometry),
    destination_place = lead(origin_place)
  ) |>
  filter(row_number() != n()) # remove last row

routes_geocoded_raw <- routes_raw %>%
  rowwise() %>%
  mutate(route = osrmRoute(
    src = origin_geometry,
    dst = destination_geometry
  ))

routes_geocoded <- routes_geocoded_raw %>%
  unnest(route, names_sep = "_") %>%
  st_set_geometry("route_geometry")

leaflet(routes_geocoded) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolylines() %>%
  addCircleMarkers(
    lat = all_stops_unique$lat,
    lng = all_stops_unique$long,
    # popup = paste(df$com,"-",format(df$time,"%H:%M:%S")),
    color = "red",
    stroke = FALSE,
    radius = 8,
    fillOpacity = 0.8
  )

library(leaflet)
library(dplyr)

# 1. Assign categories to your stops
all_stops_unique <- all_stops_unique %>%
  mutate(category = case_when(
    grepl("Airport", place, ignore.case = TRUE) ~ "airport",
    grepl("Nest|Hike|Trek", place, ignore.case = TRUE) ~ "hiking",
    grepl("Camp", place, ignore.case = TRUE) ~ "camping",
    grepl("Dzong|Temple|Monastery|Nunnery|Goenpa", place, ignore.case = TRUE) ~ "temple",
    grepl("Pass|Peak|Mountain", place, ignore.case = TRUE) ~ "nature",
    TRUE ~ "culture"
  ))

# 2. Define icons for each category
icons <- awesomeIconList(
  airport  = makeAwesomeIcon(icon = "plane", library = "fa", markerColor = "blue"),
  hiking   = makeAwesomeIcon(icon = "male", library = "fa", markerColor = "green"),
  camping  = makeAwesomeIcon(icon = "fire", library = "fa", markerColor = "orange"),
  temple   = makeAwesomeIcon(icon = "building", library = "fa", markerColor = "red"),
  nature   = makeAwesomeIcon(icon = "leaf", library = "fa", markerColor = "darkgreen"),
  culture  = makeAwesomeIcon(icon = "camera", library = "fa", markerColor = "purple")
)

# 3. Build the map
leaflet(routes_geocoded) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  # Route lines, coloured by day
  addPolylines(
    color = ~ colorFactor("RdYlBu", domain = routes_geocoded$day)(day),
    weight = 3,
    opacity = 0.8,
    popup = ~ paste(
      "Day", day, "<br>", origin_place, "→", destination_place,
      "<br>Distance:", round(route_distance, 1), "km",
      "<br>Duration:", round(route_duration, 1), "mins"
    )
  ) %>%
  # Place markers with category icons
  addAwesomeMarkers(
    data = all_stops_unique,
    lat = ~lat,
    lng = ~long,
    icon = ~ icons[category],
    label = ~place, # hover label = place name
    popup = ~ paste0(
      "<b>", place, "</b><br>",
      "Day: ", day, "<br>",
      "Category: ", category
    )
  ) %>%
  # Place name labels (permanent, not just on hover)
  addLabelOnlyMarkers(
    data = all_stops_unique,
    lat = ~lat,
    lng = ~long,
    label = ~place,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size"   = "11px",
        "color"       = "white",
        "text-shadow" = "1px 1px 2px black"
      )
    )
  ) %>%
  # Legend for icon categories
  addLegend(
    position = "bottomright",
    colors   = c("blue", "green", "orange", "red", "darkgreen", "purple"),
    labels   = c("Airport", "Hiking", "Camping", "Temple/Monastery", "Nature", "Culture"),
    title    = "Place type",
    opacity  = 0.8
  )

ggplot() +
  geom_sf(data = bhutan_map) +
  geom_sf(data = routes_geocoded, color = clrs[1]) +
  geom_sf(data = all_stops_unique) +
  geom_label_repel(
    data = all_stops_unique,
    aes(label = place, geometry = geometry),
    stat = "sf_coordinates", seed = 1234,
    size = 3, segment.color = clrs[3], min.segment.length = 0
  ) +
  annotation_scale(
    location = "bl", bar_cols = c("grey30", "white"),
    unit_category = "imperial", text_family = "Overpass"
  ) +
  coord_sf(crs = st_crs("ESRI:102003"))

bbox <- st_bbox(routes_geocoded)

bbox_buffer <- routes_geocoded |>
  st_bbox() |>
  st_as_sfc() |> # convert to sf object
  st_buffer(50) |>
  st_transform("ESRI:102003") |> # Change to Albers projection, better for regional mapping
  st_bbox() # extract expanded box

ggplot() +
  geom_sf(data = bhutan_map) +
  geom_sf(data = routes_geocoded, color = clrs[1]) +
  geom_sf(data = all_stops_unique) +
  geom_sf_label(
    data = all_stops_unique,
    aes(label = place),
    # We're in Albers again, so we can work with meters (or miles)
    nudge_y = 1
  ) +
  annotation_scale(
    location = "bl", bar_cols = c("grey30", "white"),
    unit_category = "imperial", text_family = "Overpass",
    width_hint = 0.4
  ) +
  scale_fill_manual(values = c("grey90", "grey80"), guide = "none") +
  coord_sf(
    xlim = c(bbox_buffer["xmin"], bbox_buffer["xmax"]),
    ylim = c(bbox_buffer["ymin"], bbox_buffer["ymax"]),
    crs = st_crs("ESRI:102003")
  )



# Make an interactive map
leaflet(stops_geocoded) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(~long, ~lat)
# convert to sf spatial object and set the coordinate ref system to EPSG:4326
st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

coords <- read_exif("photos/",
  tags = c("FileName", "GPSLatitude", "GPSLongitude", "DateTimeOriginal")
)

image_files <- list.files(system.file("photos/", package = "exifr"), full.names = TRUE)

stops_to_geocode <- stops_raw |>
  left_join(stops_addresses, by = join_by(city)) %>%
  # Combine the address and city columns, with a preference for address
  mutate(address = coalesce(address, city))
