pacman::p_load(
  "rgdal",
  "rgeos",
  "RColorBrewer",
  "viridis",
  "scales",
  "rmapshaper",
  "naniar"
)

map_br <- readOGR(
  here("data/maps/"),
  "municipio"
)

map_br <- SpatialPolygonsDataFrame(
  gSimplify(map_br, tol = 0.01, topologyPreserve = T),
  data = map_br@data
)

map_br@data <- map_br@data %>%
  mutate(id = row.names(map_br))

map_br <- map_br %>%
  fortify %>%
  left_join(., map_br@data, by = "id") %>%
  transmute(
    long,
    lat,
    group,
    cod_ibge_6 = as.numeric(levels(cod_ibge_6))[cod_ibge_6],
    mun_name = sem_acento
  )

# state boundaries
# map_state <- readOGR(
#   here("data/maps/"),
#   "estados_2010"
# ) %>%
#   fortify