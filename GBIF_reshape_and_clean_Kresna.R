library(tidyverse)
library(sf)
library(leaflet)

# Options
old <- options(pillar.sigfig = 7)
tibble(x = 123.4567)

# # Restore old options, see also rlang::local_options() for a more elegant way
# options(old)

setwd('G:/My Drive/6.Projects/GBIF/Data mobilization/Roadkill Kresna')
getwd()

## Roadkill Kresna

#source: nmnhs-monitoring-2003-2006_modif.xlsx
#original_file here"

taxa <- read_delim('Checklist_GBIF_normalized.txt', 
                   delim = '\t',
                   col_names = T)
taxa


# Clean the taxa in the occurrence list
# File available here:
#https://nmnhs.com/downloads/varia/nmnhs-monitoring-2003-2006.xlsx

occurrence <- readxl::read_xlsx('nmnhs-monitoring-2003-2006.xlsx', 
                                sheet = "Data base", trim_ws = T,
                                col_types = "text")
occurrence

occurrence <- select(occurrence, 
                     Year:Age,
                     sectorID = `Location (identification number of monitoring section)`,
                     Distance_banquet_m = `Distance from banquet`,
                     RoadSide_Directions = `Direction (road side) - W/E`,
                     RoadSide_Biotop = `Direction (road side) - Slope/River`)

occurrence$Species <- gsub(x = occurrence$Species, 
                           pattern = "[^\u0001-\u007F]+|<U\\+\\w+>", 
                           repl = "") #. This works!

occurrence %<>%
  mutate(Genus = sub(Genus, patt = "Bat", repl = "", ignore.case = T)) %>%
  mutate(Species = sub(Species, patt = "-", repl = "", fixed = T)) %>%
  mutate(across(c(Species, Genus, Family, Order), 
                ~ gsub(., patt = "sp.", repl = ""))) %>%
  # mutate(Species = gsub(x = Species, patt = "<U\\+\\w+>", repl = "")) %>%
  mutate(verbTaxon = paste(Genus, Species),
         verbTaxon = str_squish(verbTaxon),
         verbTaxon = stringr::str_to_sentence(verbTaxon)) %>%
  rowwise() %>%
  mutate(verbTaxon = replace(verbTaxon, is.na(Genus) | verbTaxon == "", Family),
         verbTaxon = replace(verbTaxon, is.na(verbTaxon) | verbTaxon == "", Order),
         verbTaxon = replace(verbTaxon, is.na(verbTaxon) | verbTaxon == "", Class)) %>%
  ungroup()

occurrence %>% 
  distinct(#Class, Order, Family, 
    Genus, Species, verbTaxon) %>% 
  as.data.frame() %>% 
  arrange(#Class, Order, Family, 
    Genus, Species, verbTaxon)


# Create a footprintWKT and calculate sampleSizeValue	sampleSizeUnit
# Done by Popgeorgive

coords_transects <- readxl::read_xlsx('nmnhs-monitoring-2003-2006.xlsx',
                                      col_types ="text",
                                      sheet = "coordinates monitoring sections")
# coords_transects <- mutate(coords_transects, across(2:7, as.numeric))
coords_transects

coords_transects <- rename(coords_transects,
                           sectorID = `Location (identification number of monitoring section)`,
                           lat_2 = `Location - Latitude`,
                           long_2 = `Location - Longitude`,
                           lat_1 = `Location - Latitude (north part of monitoring section)`,
                           long_1 = `Location - Longitude  (north part of monitoring section)`,
                           lat_3 = `Location - Latitude (south part of monitoring section)`, 
                           long_3 = `Location - Longitude  (south part of monitoring section)`)

coords_transects <-
  select(coords_transects, sectorID, contains("1"), contains("2"), contains("3")) 
coords_transects

coords_transects %>%
# occurrence %>%
  mutate(across(lat_1:long_3, as.numeric)) %>%
  summary()


# Are all ID coinciding?
coords_transects %>%
  filter(!sectorID %in% occurrence$sectorID)
coords_transects <- mutate(coords_transects, sectorID = tolower(sectorID))

occurrence %>%
  filter(!sectorID %in% coords_transects$sectorID) %>%
  distinct(sectorID)
# Conclusion: Difference in the capital letter
occurrence <- mutate(occurrence, sectorID = tolower(sectorID))

# Reshape in a longf-format suitable for polylines
coord_reshaped <- 
  coords_transects %>%
  select(sectorID, lat_1:long_3) %>%
  pivot_longer(cols = -1, values_to = "value") %>%
  separate(col = name,sep = "_", into = c("coord", "position")) %>%
  pivot_wider(names_from = coord, values_from =  value) %>%
  mutate(across(lat:long, as.numeric)) %>%
  mutate(label = paste(sectorID, position, sep = "-"))
coord_reshaped

# Add color and radius
coord_reshaped <- 
  mutate(coord_reshaped,
         color = rep(rep(c("green", "red", "blue"), 
                         each = 3), n_distinct(coord_reshaped$sectorID)/3),
         radius = rep(rep(1:3, each = 3), 
                      n_distinct(coord_reshaped$sectorID)/3))
coord_reshaped

# Check the points and lines
myMap <-
  leaflet(coord_reshaped) %>%
  addTiles()  # Add default OpenStreetMap map tiles
myMap

for(group in unique(coord_reshaped$sectorID)){
  myMap <- addPolylines(myMap, 
                        lng=~long,
                        lat=~lat,
                        data=coord_reshaped[coord_reshaped$sectorID==group,], 
                        label = group,
                        labelOptions = labelOptions(interactive = T),
                        color= ~color)
}
myMap

for(group in unique(coord_reshaped$position)){
  myMap <- addCircleMarkers(myMap, 
                            lng = ~long,
                            lat = ~lat,
                            data = coord_reshaped[coord_reshaped$position==group,],
                            label = ~label,
                            labelOptions = labelOptions(interactive = T),
                            color = ~color, 
                            radius = ~radius)
}
myMap


# Correct 34 and 35 with a manually # Arbitrary chosen on Google Maps to be between the 2 sectors
coord_reshaped <-
  coord_reshaped %>%
  mutate(lat = replace(lat, 
                       sectorID == "35" & position == "1",
                       41.7958678), 
         long = replace(long, 
                        sectorID == "35" & position == "1",
                        23.1578503),  # Arbitrary chosen on Google Maps
         lat = replace(lat, 
                       sectorID == "34" & position == "3",
                       41.7958678),
         long = replace(long,
                        sectorID == "34" & position == "3",
                        23.1578503))

# Correct manually 18, 20, 18b
coord_reshaped %>%
  filter(sectorID %in% grep(sectorID, patt = "18", fixed = T, value = T) |
           sectorID %in% c("19", "20")) %>%
  arrange(sectorID, position)
# Conclusion: Sector 18-3, 18b-3 and 20-1 are overlapping. Change to 41.8189808N, 23.1576869E

coord_reshaped <-
  coord_reshaped %>%
  mutate(lat = replace(lat, 
                       sectorID == "18" & position == "3",
                       41.818981), # Arbitrary chosen on Google Maps
         long = replace(long, 
                        sectorID == "18" & position == "3",
                        23.157687),  # Arbitrary chosen on Google Maps
         lat = replace(lat, 
                       sectorID == "18b" & position == "3",
                       41.818981),
         long = replace(long,
                        sectorID == "18b" & position == "3",
                        23.157687),
         lat = replace(lat, 
                       sectorID == "20" & position == "1",
                       41.818981),
         long = replace(long,
                        sectorID == "20" & position == "1",
                        23.157687))


## Combine 18a and 18b as together they make sector 18 
# occurrence <- occurrence %>%
#   rowwise() %>%
#   mutate(sectorID = replace(sectorID, sectorID %in% c("18a", "18b"), "18")) %>%
#   ungroup()



# Extract the footprint
library(sf)

coord_reshaped %<>%
  group_by(sectorID) %>%
  mutate(footprintWKT = st_linestring(matrix(c_across(long:lat),
                                             ncol=2,
                                             byrow=F)) %>% 
           st_as_text) %>%
  ungroup()
coord_reshaped

# convert the column "footprintWKT" to sfc
sf_footprintWKT <- distinct(coord_reshaped %>% select(sectorID, footprintWKT) %>% ungroup())
sf_footprintWKT <- st_as_sfc(sf_footprintWKT$footprintWKT)

# give the data frame with sf class
sf_footprintWKT <- st_sf(sf_footprintWKT, crs=4326) #set the CRS to be 4326 (WGS 84)
sf_footprintWKT


library(ggspatial)
ggplot()+
  geom_sf(data=sf_footprintWKT)+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tl")



# Calculate the distance (not along the road)
coord_reshaped <-
  mutate(coord_reshaped, PID = rep(1:(nrow(coord_reshaped)/3), each = 3))
coord_reshaped

library(PBSmapping)
pline <- as.PolySet(coord_reshaped %>% 
                      select(X = long, Y = lat, PID, POS = position) %>%
                      mutate(X = as.numeric(X), Y = as.numeric(Y),
                             POS = as.integer(POS)) %>%
                      as.data.frame(), 
                    projection = 1)
plotLines(pline,col = c("red", "green"))

coord_reshaped <- coord_reshaped %>% left_join(calcLength(pline))

# Add the lengths calculated by Georgi
footprintWKT_GP <- readxl::read_xlsx("wktExport.xlsx",sheet = 1)
footprintWKT_GP <- mutate(footprintWKT_GP, sectionID = tolower(sectionID))

# Combine the coordinates
coord_reshaped <- left_join(coord_reshaped, 
                            footprintWKT_GP %>% select(-OBJECTID), 
                            by = c('sectorID' = 'sectionID'))

coord_reshaped %>%
  filter(position == "2") %>%
  distinct() %>%
  filter(long != x | lat != y)

coord_reshaped <-
  coord_reshaped %>% 
  mutate(SampleSize = Shape_Leng) %>%
  rowwise() %>%
  mutate(SampleSize = replace(SampleSize, x == 0, length*10^5)) %>%
  ungroup()

coord_reshaped %>%
  filter(position == "2") %>%
  distinct() %>%
  filter(x == 0) %>%
  glimpse()

comb <- 
  occurrence %>%
  mutate(verbTaxon = str_to_sentence(verbTaxon)) %>%
  left_join(taxa %>% 
              mutate(OriginalName = str_squish(OriginalName)), 
            by = c('verbTaxon' = "OriginalName")) %>%
  left_join(coord_reshaped %>%
              filter(position == "2") %>%
              select(sectorID,
                     decimalLatitude = lat, 
                     decimalLongitude = long,
                     footprintWKT,
                     sampleSizeValue = SampleSize) %>%
              distinct()
            )
comb %>% glimpse()

# Is there any original taxon without a match in GBIF's taxonomic backbone?
comb %>%
  filter(is.na(scientificName)) %>%
  distinct(Taxon) %>% as.data.frame()

# Export the dataset
comb %>%
  mutate(occurrenceID = 1:n()) %>%
  # Record-level
  mutate(type = "Event",
         language = "en",
         license = "http://creativecommons.org/licenses/by/4.0/legalcode",
         rightsHolder = "National Museum of Natural History, Sofia",
         accessRights = "not-for-profit use only, data citation, with link to tdata source, required",
         institutionID = "https://ror.org/04a4v0j95",
         institutionCode = "NMNHS",
         datasetName = "Roadkill in Kresna Gorge 2003-2006",
         ownerInstitutionCode = "NMNHS",
         basisOfRecord = "HumanObservation") %>%
  # Occurrence
  mutate(recordedBy = "info lost at occurrence level, see the metadata instead",
         individualCount = 1,
         organismQuantity = 1,
         organismQuantityType  = "individuals",
         vitality = "dead", # not in the IPT yet
         lifeStage = tolower(Age),
         establishmentMeans = "native (indigenous)",
         degreeOfEstablishment = "native (category A)",
         pathway = "unaided",
         georeferenceVerificationStatus = "verified by contributor",
         ) %>%
  rowwise() %>%
  mutate(georeferenceVerificationStatus = replace(georeferenceVerificationStatus,
                                                  sectorID %in% c("18, 18b", "20", "34","35"),
                                                  "requires verification")) %>%
  ungroup() %>%
  mutate(occurrenceStatus = "present") %>%
  # Event
  mutate(samplingProtocol = 'ad hoc observation | point count of dead animals',
         sampleSizeUnit = "metre",
         verbatimLocality = "Kresna Gorge",
         samplingEffort = 'whole transect by foot by 2 observers') %>%
  # Location
  mutate(continent = "Europe",
         countryCode = "BG",
         municipality = "Kresna",
         geodeticDatum = "WGS84",
         coordinateUncertaintyInMeters = round(sampleSizeValue/2,0)+20,
         georeferencedBy = "Andrey Kovachev",
         georeferencedDate = "2010/2015",
         georeferenceProtocol = "The first, mid and end point of each section are georeferenced ad hoc using Google Earth",
         georeferenceSources = "Google Earth") %>%
  rowwise() %>%
  mutate(georeferencedBy = replace(georeferencedBy,
                                  sectorID %in% c("18, 18b", "20", "34","35"),
                                  "Salza Palpurina"),
         georeferencedDate = replace(georeferencedDate,
                                   sectorID %in% c("18, 18b", "20", "34","35"),
                                   "2024/02/20")) %>%
  ungroup() %>%
  # TAxon
  mutate(taxonID = paste0("https://www.gbif.org/species/", key),
         taxonRank = tolower(rank)) %>%
  select(eventID = sectorID, 
         originalNameUsageID = key,
         taxonomicStatus = status,
         acceptedNameUsageID = acceptedUsageKey,
         acceptedNameUsage = species,
         occurrenceID, basisOfRecord, 
         contains("decim"),
         coordinateUncertaintyInMeters,
         contains("sample"), footprintWKT, everything()) %>%
  select(-Age) %>%
  write_delim("roadkill_Kresna_polished_taxonomy_and_coords.txt", delim = '\t')

