## Data wrangling of eBird data for Santa Clara and San Mateo county
# Alejandro Brambila, University of Oregon
# November 2018
#===========================
# Part II: DATA PREPARATION
#==========================
#now I can just work directly with the SC files I created in "eBird_pre_prep_2018.R"

library(tidyverse)
library(lubridate)
SC2002 <- read_csv("SC2002") 
SC2003 <- read_csv("SC2003") 
SC2004 <- read_csv("SC2004") 
SC2005 <- read_csv("SC2005") 
SC2006 <- read_csv("SC2006") 
SC2007 <- read_csv("SC2007") 
SC2008 <- read_csv("SC2008") 
SC2009 <- read_csv("SC2009") 
SC2010 <- read_csv("SC2010") 
SC2011 <- read_csv("SC2011") 
SC2012 <- read_csv("SC2012") 
SC2013 <- read_csv("SC2013") 
SC2014 <- read_csv("SC2014") 
SC2015 <- read_csv("SC2015") 
SC2016 <- read_csv("SC2016") 

# bind the years togehter
bound<-rbind(SC2002, 
             SC2003, 
             SC2004, 
             SC2005, 
             SC2006, 
             SC2007, 
             SC2008, 
             SC2009, 
             SC2010, 
             SC2011, 
             SC2012, 
             SC2013, 
             SC2014, 
             SC2015, 
             SC2016)

# Test for replicated IDs (also did this by year and tested for overlap of year files)
test<-bound %>% 
  group_by(SAMPLING_EVENT_ID) %>% 
  summarize(sum=n()) %>% 
  arrange(desc(sum))

## Change X in species data to 1, NA to zero, and convert all to integers
bound[,20:4670][(bound[,20:4670])=="X"] <- 1
bound[,20:4670][is.na(bound[20:4670])] <- 0
bound[,20:4670] <- lapply(bound[,20:4670], as.integer)


## Remove species not found (only 344 remain of initial 4650)
spectot<-as.tibble(colSums(bound[20:4670], na.rm=TRUE))
spectot<-rownames_to_column(spectot, var="species") %>%
  filter(value!=0)
species<-spectot$species
bound<- select(bound, 1:19, species)

## remove repeat counts for same group ID, leave only primary checklist
SC <- bound %>%
  filter(PRIMARY_CHECKLIST_FLAG==1) %>%
  mutate(GROUP_ID=ifelse(GROUP_ID=="?", "none", GROUP_ID))%>%
  # remove everything but stationary counts, traveling counts, and area searches
  filter(COUNT_TYPE %in% c("P21","P22","P23","P62", "P35", "P40"))

# Create species list based on new data
speciesnames<-read_csv("./ERD2016/doc/taxonomy.csv")%>%
  mutate(species=SCI_NAME)
species<-left_join(spectot, speciesnames)%>%
  select(PRIMARY_COM_NAME, SCI_NAME, value)%>%
  arrange(desc(value))

###############Species TODO: Code species by oak association, urban/rural

# Visualize most observed species
ggplot(subset(species, value>10000)) +
  geom_bar(aes(reorder(PRIMARY_COM_NAME, -value), value), stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Common Name") +ylab("Total Count") +
  ggtitle("Species with counts over 10,000 in Santa Clara")

rm(bound, SC2002, SC2003, SC2004, SC2005, SC2006, 
   SC2007, SC2008, SC2009, SC2010, SC2011, SC2012,SC2013, SC2014, 
   SC2015, SC2016, spectot, speciesnames)


#remove unnecessary columns, rename sampling type
SC <-SC%>%
  select(-COUNTRY, -STATE_PROVINCE, -PRIMARY_CHECKLIST_FLAG, -GROUP_ID, 
         -OBSERVER_ID, -EFFORT_AREA_HA, -COUNTY, -MONTH, -DAY, -TIME)%>%
  mutate(COUNT_TYPE=ifelse(COUNT_TYPE=="P21", "stationary", 
                           ifelse(COUNT_TYPE=="P22", "traveling", 
                                  ifelse(COUNT_TYPE=="P23", "exhautive", 
                                       ifelse(COUNT_TYPE=="P35", "yard",
                                              ifelse(COUNT_TYPE=="P40", "styard", "historical"))))))

#Create  dataframe with just checklist info
SClists <- SC[1:9] 

# Create dataframe with species matrix (counts), 
# keeps a column "SAMPLING_EVENT_ID" to join to SClists
SCcounts<-SC[-2:-9] 

# Create a dataframe with species matrix (presence/absence), 
# keeps a column "SAMPLING_EVENT_ID" to join to SClists
SCpa<-SCcounts %>%
  mutate(rowname=SAMPLING_EVENT_ID)
SCpa<-column_to_rownames(SCpa)
SCpa<-SCpa%>%
  select(-SAMPLING_EVENT_ID)
SCpa[SCpa>0] <-1
SCpa<-rownames_to_column(SCpa)%>%
  rename(SAMPLING_EVENT_ID=rowname)


# ADD species richness, make final database
sprichness<-mutate(SCpa, richness=rowSums(SCpa[2:343]))%>%
  select(SAMPLING_EVENT_ID, richness)
SClists<-left_join(SClists, sprichness)

# visualize histogram of richness
ggplot(SClists, aes(richness)) +geom_histogram(binwidth=1) +ggtitle("Checklist species richness frequency in Santa Clara")

# visualize num of checklists/year
ggplot(SClists) + geom_histogram(aes(x=YEAR), stat="count", binwidth=1) +ggtitle("Checklists by year")


# Check for replicated checklists 
test<-SC %>% 
  group_by(SAMPLING_EVENT_ID) %>% 
  summarize(sum=n()) %>% 
  arrange(desc(sum))
count(test, sum)

# Save final data as csv
write.csv(species, "eBird_SC_species_2018.csv")     #species codes
write.csv(SCcounts, "eBird_SC_counts_2018.csv")    #count matrix (with ID)
write.csv(SCpa, "eBird_SC_pa_2018.csv")        #presence matrix (with ID)
write.csv(SClists, "eBird_SC_lists_2018.csv")     # checklist info
