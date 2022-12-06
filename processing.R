install.packages("tidyverse")
install.packages("sf")
install.packages("foreign")
install.packages('terra', repos='https://rspatial.r-universe.dev')
install.packages("networkD3")
install.packages("htmlwidgets")

library(tidyverse)
library(sf)
library(terra)
library(ggplot2)

##############
# READ DATA
c <- read.csv("./outputs/bps2evt.csv")

##############
##############
# PROCESSING FOR SANKEY

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

# selfcontained=T is depreciated. Interesting, might cause problems
htmlwidgets::saveWidget(snky, "./outputs/sankey1.html", selfcontained = T)





####################
####################
# VDEP MAP
v_att <- read.csv("./LF20_VDep_220.csv")
