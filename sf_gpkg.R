# install

remotes::install_github("r-spatial/sf@gpkg")

# creating gpkg file with 2 spatial layers

library(dplyr)
library(sf)

inborutils::download_zenodo("10.5281/zenodo.3784149")
inborutils::download_zenodo("10.5281/zenodo.3386815")

habsprings <- st_read("habitatsprings.geojson")
habsprings %>% st_write("layers.gpkg",
                        layer = "habsprings",
                        delete_dsn = TRUE)
(read_sf("layers.gpkg"))

sac <- st_read("sac.shp")
sac %>% st_write("layers.gpkg",
                        layer = "sac")

st_layers("layers.gpkg")
(read_sf("layers.gpkg", layer = "sac"))
(read_sf("layers.gpkg", layer = "habsprings"))

# adding non-spatial table

mydf <- tibble(a = rep(letters, 100),
               b = seq_along(a) %>% rev,
               c = b / 100,
               d = lubridate::dmy("1/1/2020") - b,
               e = lubridate::now() - b)

## without GDAL option already works beautifully (uses GDAL default):

mydf %>%
    st_write("layers.gpkg",
             layer = "mydf",
             delete_layer = TRUE)  # note: delete_layer also works after repeating

(read_sf("layers.gpkg", layer = "mydf")) # no warning
(st_read("layers.gpkg", layer = "mydf",  # returns warning = OK
         as_tibble = TRUE,
         stringsAsFactors = FALSE))

(read_sf("layers.gpkg", layer = "mydf")) %>%
    mutate(e = as.character(e)) %>%
    all.equal(mydf %>%
                  mutate(e = as.character(e)))

## explicitly using GDAL option has no other effect except explicit confirmation
## of layer_options:

mydf %>%
    st_write("layers.gpkg",
             layer = "mydf2",
             layer_options = "ASPATIAL_VARIANT=GPKG_ATTRIBUTES")

(read_sf("layers.gpkg", layer = "mydf")) %>%
    all.equal(
        (read_sf("layers.gpkg", layer = "mydf2"))
    )

## creating new gpkg file with only non-spatial layers:

mydf %>%
    st_write("layers2.gpkg",
             layer = "mydf",
             delete_dsn = TRUE)
mydf %>%
    st_write("layers2.gpkg",
             layer = "mydf2")


# again adding spatial layers to both gpkg files:

sac %>%
    filter(stringr::str_detect(NAAM, "Turnhout")) %>%
    st_write("layers.gpkg",
             layer = "turnhout")
sac %>%
    filter(stringr::str_detect(NAAM, "Turnhout")) %>%
    st_write("layers2.gpkg",
             layer = "turnhout")

st_layers("layers.gpkg")
st_layers("layers2.gpkg")

(read_sf("layers2.gpkg", layer = "turnhout"))
(read_sf("layers2.gpkg", layer = "mydf"))
(read_sf("layers2.gpkg", layer = "mydf2"))

# reading GPKG with aspatial table from GPKG 1.0 also works:
#
# soil_path <-
#     file.path(n2khab::fileman_up("n2khab_data"),
#               "20_processed/soilmap_simple/soilmap_simple.gpkg")
# st_layers(soil_path)
# (read_sf(soil_path))
# (read_sf(soil_path, layer = "explanations")) # no warning
# (st_read(soil_path, layer = "explanations",  # returns warning = OK
#          as_tibble = TRUE,
#          stringsAsFactors = FALSE))


