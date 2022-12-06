library(tidyverse)
library(terra)
library(tmap)
library(raster)

r <- rast("../../mi_ecosystems/outputs/bps_michigan.tif")

re <- resample(x = r, y = aggregate(r, fact = 11), method = "near")
plot(re)

activeCat(re) <- "GROUPVEG"

activeCat(r) <- "GROUPVEG"
writeRaster(r, "../../mi_ecosystems/outputs/bps_michigan_group.tif")
write.dbf(bps_att, paste0("./outputs/bps_", aoi_filename, ".tif.vat.dbf"))


re_r <- raster(re)
qtm(re_r)
ttm()
