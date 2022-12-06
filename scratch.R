# this is a file full of scratch code
# first I will see how it updates to github
# then I will explore the code using the terra package

# most recent version is on devtools
install.packages('terra', repos='https://rspatial.r-universe.dev')

library(tidyverse)
library(terra)
library(sf)

################################
## Reading, cropping, masking, writing the raster


# import the michigan counties shapefile from disk
mico <- st_read("C:/Users/mwali/mwalimaa/Projects/SpatialData/Michigan/Counties_v17a/Counties_v17a.shp") %>%
  st_transform(crs = 5070) %>%
  st_union() %>%
  vect()
mi <- st_read("C:/Users/mwali/mwalimaa/Projects/SpatialData/us_state_bounds/tl_2022_us_state.shp") %>%
  st_transform(5070) %>%
  filter(NAME == "Michigan") %>%
  vect()


# import the most recent landfire data and crop mask to mi
us_bps <- rast("C:/Users/mwali/mwalimaa/Projects/SpatialData/LANDFIRE/LF2020_BPS_220_CONUS/Tif/LC20_BPS_220.tif")
us_evt <- rast("C:/Users/mwali/mwalimaa/Projects/SpatialData/LANDFIRE/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif")

### save color table for later
cols <- coltab(us_bps)
saveRDS(cols, file = "./bps_coltab.RData")
saveRDS(coltab(us_evt), file = "./evt_coltab.RData")
### remove color table from raster
### coltab(us_bps) <- NULL


# crop and mask raster to the shapefile
mi_bps <- crop(us_bps, mi, mask = T)
### reassign color table to raster
### coltab(mi_bps) <- cols

plot(mi_bps) # looks good

# confirm INT2S datatype
str(mi_bps)

# write the raster
writeRaster(mi_bps, "./outputs/mi_bps_crop.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

# remove unneccessary large raster
rm(us_bps)

## Randy, were you able to export the colortable with the GeoTIF?

###################################
## extracting information

activeCat(mi_bps) <- "Value"
plot(mi_bps)

x <- values(mi_bps, dataframe = T, na.rm = T) %>%
  table(dnn = "Value") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(mi_bps)[[1]], by = "Value")

x2 <- x %>% filter(Freq != 0)

install.packages("foreign")

# clean memory
gc()
# foreign package write as dbf file, same name as .tif but .tif.vat.dbf

foreign::write.dbf(x2, "./outputs/mi_bps_crop.tif.vat.dbf")


##########################################################
### write bounding box and geojson and upload to LF

  st_write("./outputs/aoi_bbox.geojson", delete_dsn = T)
