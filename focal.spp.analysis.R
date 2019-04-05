library(tidyverse)
# 1. Table of focal species
# 2. Counts of focal species
# 3. Aggregation of checklists in the same location

<-read_csv(species, "eBird_SC_species_2018.csv")     #species codes
<-read_csv(SCcounts, "eBird_SC_counts_2018.csv")    #count matrix (with ID)
read_csv(SCpa, "eBird_SC_pa_2018.csv")        #presence matrix (with ID)
read_csv(SClists, "eBird_SC_lists_2018.csv")     # checklist info



# Table of key oak specialists and generalits (to be reviewed by Erica): focal.species
oak.specialists <- subset(species, PRIMARY_COM_NAME %in% #go through this list to only keep cavity nesters
                            c("Acorn_Woodpecker",
                              "Oak_Titmouse",
                              "Hutton's_Vireo",
                              "Nuttall's_Woodpecker",
                              "California_Scrub-Jay",
                              "Western_Bluebird",
                              "Violet-green_Swallow",
                              "Northern_Rough-winged_Swallow",
                              "California_Quail"))%>%
  mutate(niche="oak-specialist")
generalists <- subset(species, PRIMARY_COM_NAME %in% #go through this list to only keep cavity nesters
                        c("Canada_Goose",
                          "European_Starling",
                          "House_Finch",
                          "American_Robin",
                          "Rock_Pigeon",
                          "American_Crow",
                          "Dark-eyed_Junco",
                          "White-crowned_Sparrow",
                          "Bushtit"))%>%
  mutate(niche="generalist")

focal.species<-rbind(oak.specialists, generalists)
rm(oak.specialists, generalists)

#Subset of Santa Clara counts with only focal species: focal.SCcounts
focal.SCcounts <- SCcounts%>%
  select(1, focal.species$SCI_NAME)

#Subset of Santa Clara presence/absence with only focal species: focal.SCpa
focal.SCpa <- SCpa%>%
  select(1, focal.species$SCI_NAME)

#Aggregate SClists by location
lists.agg <- SClists %>%
  group_by(LATITUDE, LONGITUDE, COUNT_TYPE)%>%
  summarize(nlists=n(), recent.yr=max(YEAR), meaneffort=mean(EFFORT_HRS), 
   meankm=mean(EFFORT_DISTANCE_KM), meanobs=mean(NUMBER_OBSERVERS), maxobs=max(NUMBER_OBSERVERS), meanrich=mean(richness))

write.csv(lists.agg, "eBird_SC_locations_2018.csv")     #aggregated locations

L1151543<-subset(SClists, LOC_ID=="L1151543") #tons of stationary across a few years, usually not too many people - weird
