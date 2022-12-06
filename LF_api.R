### LANDFIRE API

install.packages("tidyverse")
install.packages("sf")
install.packages("httr")
install.packages("foreign")
install.packages('terra', repos='https://rspatial.r-universe.dev')
install.packages("networkD3")
install.packages("plotly")
install.packages("reactable")
install.packages("flexdashboard")
install.packages("stars")

library(tidyverse)
library(sf)
library(httr)
library(terra)
library(foreign)
library(plotly)
library(stars)

#####################################
#####################################

## THINGS YOU EDIT

products <- c("220BPS", "220EVT", "220VDEP", "220SCLASS") # code only works for bps and evt right now
# https://lfps.usgs.gov/helpdocs/productstable.html

shp <- st_read("C:/Users/mwali/mwalimaa/Projects/SpatialData/us_state_bounds/tl_2022_us_state.shp") %>%
  filter(NAME == "Michigan")

aoi_filename <- "michigan"

#####################################
#####################################

#### ONCE EVERYTHING ABOVE IS GOOD
#### HIGHLIGHT EVERTHING BELOW AND LET IT RIP

#####################################
## SETUP

dir.create("./outputs")
dir.create("./downloads/aoi", recursive = T)

#####################################
### ACCESSING API AND DOWNLOADING RELEVANT DATA

# create bounding box for clipping tool
bbox <- shp %>%
  st_transform(4326) %>% # lat long WGS84
  st_bbox() %>%
  unname()

# other URL aspects to create
base <- "https://lfps.usgs.gov/arcgis/rest/services/LandfireProductService/GPServer/LandfireProductService/submitJob?"
ll <- paste0("Layer_List=", paste(products, collapse = ";"))
aoi <- paste0("Area_of_Interest=", paste(bbox, collapse = "%20"))

# now we blend to one url
base_url <- paste0(base, ll, "&", aoi)

# submit job
pull_url <- GET(base_url) # after running it takes like 2 mins to do
# pulls job id
job_id <- str_extract(pull_url$url, ".{33}$")
# creates download URL
dwl_url <- paste0("https://lfps.usgs.gov/arcgis/rest/directories/arcgisjobs/landfireproductservice_gpserver/", job_id, "/scratch/", job_id, ".zip")
# creates destination filename
dest <- "./downloads/aoi.zip"


### DOWNLOADS FILE
### NEEDS TO WAIT A FEW MINUTES
### 5 MINUTE LOCK MWAHAHAHAHA go drink a beer
Sys.sleep(300)
download.file(dwl_url, "./downloads/aoi.zip")

# unzips downloaded data
unzip(dest, exdir = "./downloads/aoi")

# renames file
file.rename(paste0("./downloads/aoi/", job_id, ".tif"), paste0("./downloads/aoi/", aoi_filename, ".tif"))
file.rename(paste0("./downloads/aoi/", job_id, ".tfw"), paste0("./downloads/aoi/", aoi_filename, ".tfw"))
file.rename(paste0("./downloads/aoi/", job_id, ".tif.aux.xml"), paste0("./downloads/aoi/", aoi_filename, ".tif.aux.xml"))

# removes original .zip
file.remove("./downloads/aoi.zip")

# reads raster stact into R
r <- rast(paste0("./downloads/aoi/", aoi_filename, ".tif"))
r # notice there is no attribute table or color table


##########
##########
# BPS

bps <- r[["US_220BPS"]] %>% mask(project(vect(shp), r[["US_220BPS"]]))

levels(bps)[[1]] <- read.csv("./LF20_BPS_220.csv")
coltab(bps) <- readRDS("./bps_coltab.RData")
activeCat(bps) <- "VALUE"
plot(bps)

bps_att <- values(bps, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps)[[1]], by = "VALUE") %>%
  filter(Freq != 0)

# clear unused memory/ temporary files
gc()

# write it
writeRaster(bps, paste0("./outputs/bps_", aoi_filename, ".tif"),
          gdal = c("COMPRESS=NONE", "TFW=YES"),
          datatype = "INT2S",
          overwrite = T)
write.dbf(bps_att, paste0("./outputs/bps_", aoi_filename, ".tif.vat.dbf"))
write.csv(bps_att, paste0("./outputs/bps_", aoi_filename, ".csv"))


##########
##########
# EVT

evt <- r[["US_220EVT"]] %>% mask(project(vect(shp), r[["US_220EVT"]]))

levels(evt)[[1]] <- read.csv("./LF20_EVT_220.csv")
coltab(evt) <- readRDS("./evt_coltab.RData")
activeCat(evt) <- "VALUE"
plot(evt)

evt_att <- values(evt, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(evt)[[1]], by = "VALUE") %>%
  filter(Freq != 0)

# clear unused memory/ temporary files
gc()

# write it
writeRaster(evt, paste0("./outputs/evt_", aoi_filename, ".tif"),
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)
foreign::write.dbf(evt_att, paste0("./outputs/evt_", aoi_filename, ".tif.vat.dbf"))
write.csv(evt_att, paste0("./outputs/evt_", aoi_filename, ".csv"))

##############
##############
# VDEP

vdep <- r[["US_220VDEP"]] %>% mask(project(vect(shp), r[["US_220VDEP"]]))

levels(vdep)[[1]] <- read.csv("./LF20_VDep_220.csv")
#coltab(evt) <- readRDS("./evt_coltab.RData")
activeCat(vdep) <- "LABEL"
plot(vdep)

# vdep_att <- values(vdep, dataframe = T, na.rm = T) %>%
#   table(dnn = "VALUE") %>%
#   as.data.frame() %>%
#   mutate_all(as.character) %>%
#   mutate_all(as.integer) %>%
#   left_join(cats(evt)[[1]], by = "VALUE") %>%
#   filter(Freq != 0)

# clear unused memory/ temporary files
gc()

# write it
writeRaster(vdep, paste0("./outputs/vdep_", aoi_filename, ".tif"),
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)
# foreign::write.dbf(vdep_att, paste0("./outputs/vdep_", aoi_filename, ".tif.vat.dbf"))
# write.csv(vdep_att, paste0("./outputs/vdep_", aoi_filename, ".csv"))

##############
##############
# SCLASS

sclass <- r[["US_220SCLASS"]] %>% mask(project(vect(shp), r[["US_220SCLASS"]]))

levels(sclass)[[1]] <- read.csv("./LF20_SCla_220.csv")
#coltab(evt) <- readRDS("./evt_coltab.RData")
activeCat(sclass) <- "LABEL"
plot(sclass)

# format map the way i want, A-E assumes still in succession, otherwise changed
sclass_re <- levels(sclass)[[1]] %>%
  mutate(change = if_else(VALUE %in% c(1:5), "NatVeg", "Other")) %>%
  mutate(change = if_else(VALUE < 0 | VALUE == 111, "NotMapped", change)) %>%
  mutate(change = if_else(VALUE == 120, "Developed", change)) %>%
  mutate(change = if_else(VALUE == 180, "Agricultural", change))
levels(sclass)[[1]] <- sclass_re
activeCat(sclass) <- "change"
plot(sclass)

# create color table
value <- levels(sclass)[[1]]$VALUE
red <- c(0, 33, 33, 33, 33, 33, 205, 205, 0, 205, 81, 205, 230, 0)
green <- c(0, 167, 167, 167, 167, 167, 115, 115, 0, 115, 81, 115, 204, 0)
blue <- c(0, 0, 0, 0, 0, 0, 31, 31, 0, 31, 81, 31, 15, 0)
alpha <- c(0, 255, 255, 255, 255, 255, 255, 255, 0, 255, 255, 255, 255, 0)
s_coltab <- data.frame(value, red, green, blue, alpha)
coltab(sclass) <- s_coltab
plot(sclass, axes = F)

sclass_att <- values(sclass, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  filter(VALUE != "NotMapped")
s_sum <- sclass_att %>%
  mutate(perc = Freq / sum(pull(sclass_att, Freq)))
s_sum

# clear unused memory/ temporary files
gc()

# write it
writeRaster(sclass, paste0("./outputs/sclass_", aoi_filename, ".tif"),
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)
foreign::write.dbf(sclass_att, paste0("./outputs/sclass_", aoi_filename, ".tif.vat.dbf"))
write.csv(sclass_att, paste0("./outputs/sclass_", aoi_filename, ".csv"))

sclass <- rast("./outputs/sclass_michigan.tif")
activeCat(sclass) <- "change"
plot(sclass, axes = F)


##############
##############
# STACKING

bps <- rast("./outputs/bps_michigan.tif")
evt <- rast("./outputs/evt_michigan.tif")

names(bps) <- "BPS_V"
names(evt) <- "EVT_V"
s <- c(bps, evt)

c <- values(s, dataframe = T, na.rm = T) %>%
  plyr::count() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(bps_att, by = c("BPS_V" = "VALUE")) %>%
  left_join(evt_att, by = c("EVT_V" = "VALUE")) %>%
  filter(BPS_V > 30)
gc()
write.csv(c, "./outputs/bps2evt.csv")

##############
##############
# PROCESSING FOR SANKEY

c <- read.csv("./outputs/bps2evt.csv")

# finding GROUPVEG categories
group <- c %>% group_by(GROUPVEG) %>% 
  summarise(freq = sum(Freq.x)) %>%
  arrange(desc(freq)) %>%
  pull(GROUPVEG)

# grouping combine and filter
data <- c %>% select(GROUPVEG, EVT_PHYS, Freq.x, Freq.y, freq) %>% 
  mutate(EVT_PHYS = replace(EVT_PHYS, str_detect(EVT_PHYS, "Developed"), "Developed")) %>%
  mutate(EVT_PHYS = replace(EVT_PHYS, str_detect(EVT_PHYS, "Exotic"), "Exotic")) %>%
  mutate(EVT_PHYS = replace(EVT_PHYS, str_detect(EVT_PHYS, "Quarries"), "Mineland")) %>%
  group_by(GROUPVEG, EVT_PHYS) %>%
  summarise(freq2 = sum(freq)) %>%
  filter(GROUPVEG %in% group) %>%
  rename(source = GROUPVEG,
         target = EVT_PHYS,
         value = freq2) %>%
  mutate(source = paste0(source, " (past)"),
         target = paste0(target, " (present)"))

##############
##############
# BUILDING SANKEY

nodes <- data.frame(
  name = c(as.character(data$source),
           as.character(data$target)) %>% unique())

data$IDsource <- match(data$source, nodes$name) - 1
data$IDtarget <- match(data$target, nodes$name) - 1

snky <- networkD3::sankeyNetwork(Links = data,
                                 Nodes = nodes,
                                 Source = "IDsource",
                                 Target = "IDtarget",
                                 Value = "value",
                                 NodeID = "name",
                                 fontSize = 14,
                                 iterations = 0,
                                 sinksRight = F)
snky


htmlwidgets::saveWidget(snky, "./outputs/sankey1.html")

#################
#################
# BIG DATA TABLE

t <- read.csv("./outputs/bps2evt.csv")
bps_att <- read.csv("./outputs/bps_michigan.csv")
evt_att <- read.csv("./outputs/evt_michigan.csv")

t1 <- t %>%
  group_by(BPS_NAME, EVT_NAME, GROUPVEG, EVT_PHYS) %>%
  summarise(freq = sum(freq)) %>%
  mutate(ACRES = round(freq * 900 / 4046.86)) %>%
  select(BPS_NAME, GROUPVEG, ACRES, EVT_PHYS, EVT_NAME) %>%
  arrange(desc(ACRES))
            
t2 <- reactable::reactable(t1, defaultPageSize = 10)
t2

htmlwidgets::saveWidget(t2, "./dash/table1.html")

#################
#################
# BIG CONVERSION CHART


# i think this is a combine with BPS and SCLASS
bps <- rast("./outputs/bps_michigan.tif")
bps_att <- read.csv("./outputs/bps_michigan.csv")
sclass <- rast("./outputs/sclass_michigan.tif")

activeCat(bps) <- "VALUE"
activeCat(sclass) <- "change"

names(bps) <- "BPS_V"
names(sclass) <- "SCL_V"
s2 <- c(bps, sclass)

c2 <- values(s2, dataframe = T, na.rm = T) %>%
  plyr::count() %>%
  mutate(BPS_V = as.integer(as.character(BPS_V))) %>%
  left_join(bps_att, by = c("BPS_V" = "VALUE")) %>%
  filter(BPS_V > 30)
gc()
write.csv(c2, "./outputs/bps2scl.csv")

c3 <- c2  %>%
  # group_by(BPS_NAME) %>%
  # summarise(sumf = sum(freq)) %>%
  # left_join(c2, by = "BPS_NAME") %>%
  # select(BPS_NAME, SCL_V, sumf, freq) %>%
  group_by(BPS_NAME, SCL_V) %>%
  summarise(freq = sum(freq)) %>%
  mutate(acres = round(freq * 900 / 4046.85))

#levels(c3$SCL_V) <- rev(levels(c3$SCL_V))

chart <- ggplot(data = c3, aes(fill = SCL_V, y = acres, x = reorder(BPS_NAME, acres))) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()
ichart <- ggplotly(chart, tooltip = c("x", "y", "fill", "total"))
ichart

htmlwidgets::saveWidget(ichart, "./outputs/chart1.html")

##############
##############
# BPS MAP

bps <- rast("./outputs/bps_michigan.tif")
activeCat(bps) <- "GROUPVEG"
plot(bps, axes = F)


#################
#################
# BPS CHART
bps <- rast("./outputs/bps_michigan.tif")
activeCat(bps) <- "BPS_NAME"
bps_att2 <- values(bps, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame()
gc()

bps_j <- read.csv("./LF20_BPS_220.csv") %>%
  filter(VALUE > 30) %>%
  select(BPS_NAME, GROUPVEG) %>%
  unique()

bps_att3 <- bps_att2 %>%
  rename(BPS_NAME = VALUE) %>%
  inner_join(bps_j) %>%
  filter(Freq != 0) %>%
  mutate(acres = round(Freq * 900 / 4046.86)) %>%
  arrange(desc(acres)) %>%
  select(BPS_NAME, GROUPVEG, Freq, acres)


write.csv(bps_att3, "./outputs/bps_att_chart.csv")

bps_att3 <- read.csv("./outputs/bps_att_chart.csv") %>%
  top_n(30)

chart2 <- ggplot(data = bps_att2, aes(fill = GROUPVEG, y = acres, x = reorder(BPS_NAME, acres))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 30 Historic Ecosystems in Michigan",
       x = "BPS_NAME",
       y = "Acres",
       fill = "GROUPVEG") +
  theme_light() +
  scale_fill_manual(values = c("#92c173", "#d6009c", "#4ae500", "#ceff61", "#b50000"))
ichart2 <- plotly::ggplotly(chart2, tooltip = c("x", "y", "fill", "total"))
ichart2

#################
#################
# EVT CHART
evt <- rast("./outputs/evt_michigan.tif")
evt_att2 <- values(evt, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame()
gc()

evt_att3 <- evt_att2 %>% rename(EVT_NAME = VALUE) %>%
  left_join(read.csv("./LF20_EVT_220.csv")) %>%
  filter(Freq != 0) %>%
  filter(VALUE > 0) %>%
  filter(VALUE != 7292) %>%
  mutate(acres = round(Freq * 900 / 4046.86)) %>%
  arrange(desc(acres)) %>%
  select(EVT_NAME, EVT_PHYS, Freq, acres)



write.csv(evt_att3, "./outputs/evt_att_chart.csv")

evt_att3 <- read.csv("./outputs/evt_att_chart.csv") %>%
  top_n(30)

chart3 <- ggplot(data = evt_att3, aes(fill = EVT_PHYS, y = acres, x = reorder(EVT_NAME, acres))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 30 Current Ecosystems in Michigan",
       x = "EVT_NAME",
       y = "Acres",
       fill = "EVT_PHYS") +
  theme_light()
  #scale_fill_manual(values = c("#92c173", "#d6009c", "#4ae500", "#ceff61", "#b50000"))
ichart3 <- plotly::ggplotly(chart3, tooltip = c("x", "y", "fill", "total"))
ichart3
