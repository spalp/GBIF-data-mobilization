library(DBI)
library(odbc)
library(tidyverse)
library(magrittr)
library(dplyr)
library(xml2)
library(rgbif)

# Step 1: Get header and species abundance data from the Turboveg database ----

# # Connect to the database - method 1 - not working
# con <- dbConnect(odbc::odbc(), dsn = "DBF_Connection")

# # List available drivers
# odbc::odbcListDrivers()

# # Try to guess the encoding
# iconvlist()
# readr::guess_encoding("C:/Turbowin/Data/bg_grasslands/tvhabita.dbf")

con <- dbConnect(odbc(),
                 driver = "Microsoft Access dBASE Driver (*.dbf, *.ndx, *.mdx)",  # Try this first
                 dbq = "C:/Turbowin/Data/bg_grasslands",
                 name_encoding = "windows-1252"
                 ) 
# Tried: "ISO_8859-1"  to 4
## iso_8859-1, iso_8859-1', iso_8859-2, iso_8859-3,
# LATin-1, 
#windows-1250, windows-1251, windows-1252, windows-1253, windows-1254, windows-1255, windows-1256
# CP1250, CP1251, CP1252, CP1253, CP1254
#"ASCII" , ANSI_X3.4-1968
#"x-iscii-de" 
# UTF-8, 
# UTF-16  - gieves an error!

# List available tables
dbListTables(con)

# Query the header database
header <- dbGetQuery(con, "SELECT * FROM TVHABITA WHERE DATE>'20100501' AND DATE<='20121230'")

# View the result
head(header$LOCALITY)
nrow(header)

header %<>% as_tibble()
header
header %>% glimpse()

# Query the occurrence/abundance table

# Test whether the indexing works:
#dbGetQuery(con, "SELECT * FROM TVABUND LIMIT 10")
dbGetQuery(con, "SELECT TOP 10 * FROM TVABUND")
dbGetQuery(con, "SELECT * FROM TVABUND WHERE RELEVE_NR IN (1,2,3)")

# NOTE: If this does not work, try the following solution:
# https://forums.ivanti.com/s/article/You-receive-the-following-error-when-trying-to-rehost-dbase-data-into-GMPE-Microsoft-ODBC-dBase-Driver-Index-file-not-found?language=en_US
# Resolution
# 
# [CAUSE]
# - The dbase contact set folder, the DBase folder or the root directory contains .INF files, 
# which were created when the related DBF files were at some point  opened with Microsoft Access.
# 
# [RESOLUTION]
# - Remove ALL .INF files from the dbase folders
# - Start the rehost process again.

# Ensure header$RELEVE_NR is a vector of values
releve_numbers <- paste(header$RELEVE_NR, collapse = ", ")

# Construct the SQL query
query <- paste("SELECT * FROM TVABUND WHERE RELEVE_NR IN (", releve_numbers, ")", sep="")

# Run the query
abund <- dbGetQuery(con, query)
nrow(abund)

abund %<>% as_tibble()
abund

# Disconnect when done
dbDisconnect(con)
rm(query, releve_numbers)

# Step 2: Extract the species checklist ----

# Connect to the database
con <- dbConnect(odbc(),
                 driver = "Microsoft Access dBASE Driver (*.dbf, *.ndx, *.mdx)",
                 dbq = "C:/Turbowin/species/Bulgaria",
                 name_encoding = "windows-1252"
                 )

# List available tables
dbListTables(con)

# Test
dbGetQuery(con, "SELECT TOP 10 * FROM species")

taxon_checklist <- dbGetQuery(con, "SELECT ALL * FROM species")
nrow(taxon_checklist)
head(taxon_checklist)
tail(taxon_checklist)

taxon_checklist %<>%  as_tibble()
taxon_checklist %>% filter(SHORTNAME != ABBREVIAT)


# Disconnect when done
dbDisconnect(con)

rm(con)
# Step 3: Link to data folder  ----


getwd()
folder <- "CZ_UBZ"
setwd(paste(getwd(), folder, sep = "/"))
getwd()

list.files()



# 1. Event Core ----

# NOTE: 
# DwC Event terms (preceded by ‘dwc’, e.g. dwc:eventID) should be saved to the event table.
# Humboldt Extension terms (preceded by ‘eco’, e.g. eco:protocolNames) should be saved to a separate Humboldt table.
# See: https://docs.gbif.org/survey-monitoring-quick-start/en/#updating-your-dwc-event-dataset-with-the-humboldt-extension

##  1.1 Import the extension terms ----
# Read the XML file
xml_doc <- read_xml("https://rs.gbif.org/core/dwc_event_2024-02-19.xml")

# Find namespace (if any)
ns <- xml_ns(xml_doc)
ns

# Locate <property> elements (considering namespaces)
properties <- xml_find_all(xml_doc, ".//d1:property", ns)

# Convert the extracted data into a dataframe
DwC_Event_terms <- tibble(
  term = xml_attr(properties, "name"),
  type = xml_attr(properties, "type"),
  required= xml_attr(properties, "required"),
  descr = xml_attr(properties, "description")
)
print(DwC_Event_terms, n = 97)

DwC_Event_terms %<>%
  mutate(required = replace(required, 
                            term %in% c("eventID", 
                                        "eventDate", 
                                        "samplingProtocol",
                                        "sampleSizeValue", 
                                        "sampleSizeUnit"), 
                            "true"))

DwC_Event_terms_strongly_recommended <- c("countryCode", 
                                          "parentEventID", 
                                          "samplingEffort",
                                          "locationID",
                                          "decimalLatitude", 
                                          "decimalLongitude",
                                          "geodeticDatum",
                                          "coordinateUncertaintyInMeters",
                                          "footprintWKT")

# NOTE: No terms seems required here, but the required terms are listed here:
# https://www.gbif.org/data-quality-requirements-sampling-events#dcEventID

DwC_Event_terms %>% 
  filter(required == "true")

##  1.2 Survey sampling design and event hierarchy - eventID and parentEvenID ----
header %<>%
  #mutate(eventID = paste(FIELD_NO, paste0(sprintf("%02d", SURF_AREA), "sqm"), sep = "_")) %>%
  mutate(eventID = paste(FIELD_NO, paste0(SURF_AREA, "sqm"), sep = "_")) %>%
  select(eventID, everything()) %>%
  arrange(DATE, FIELD_NO, SURF_AREA)
header %>% glimpse()

header %<>%
  group_by(FIELD_NO) %>%
  arrange(SURF_AREA, .by_group = TRUE) %>%  # Sort in descending order
  mutate(parentEventID = lead(eventID, order_by = SURF_AREA)) %>%  # Parent is the next larger plot
  ungroup() %>%
  mutate(parentEventID = replace_na(parentEventID, "SP")) %>%
  select(contains("eventID"), FIELD_NO, SURF_AREA, everything())
header

header %<>% arrange(FIELD_NO, SURF_AREA)
header %>% select(contains("eventID"), FIELD_NO, SURF_AREA)

## Add an eventID for the overall project!
eventcore <- 
  data.frame(eventID = "SP") %>%
  bind_rows(header) 

##  1.3 Survey event site ----
### 1.3.1 Site description: habitat ----
eventcore %<>%
  rowwise() %>% # At each level, without the highest, project, level
  mutate(habitat = 
           # HABITAT %>% 
           # sub( patt = "(^.+)(with|wtih)(.+$)", repl ="\\1") %>% 
           # sub(patt = "(^.+)(\\son\\s)(.+$)", repl ="\\1") %>% 
           # sub(patt = "stepe", repl ="steppes", fixed = T) %>% 
           # sub(patt = "grys", repl ="gras", fixed = T)%>% 
           # str_squish() %>% 
           # str_to_sentence()
           case_when(
             HABITAT_SH == "Steppe" ~ "Steppe",
             HABITAT_SH == "PetSte" ~ "Rocky steppe",
             HABITAT_SH == "LoesSte" ~ "Loess steppe",
             HABITAT_SH == "StepMead" ~ "Meadow steppe",
             HABITAT_SH == "MedGrass" ~ "Mediterranean grassland")
         ) %>%
  ungroup() %>%
  mutate(habitat = replace(habitat, # At the project level
                           eventID == "SP",
                           habitat %>% na.omit() %>% unique() %>% sort() %>% toString()
  )
  )
eventcore %>%
  count(habitat,sort = T)
eventcore %>% select(contains("event"), contains("hab",ignore.case = F))


### 1.3.2. Site locality: loc..ID, countryCode, dec.Long., dec.Lat., .datum, locality ----
# Strongly recommended terms:
# Source: https://docs.gbif.org/survey-monitoring-quick-start/en/#site-locality

eventcore %<>%
  mutate(locationID = FIELD_NO,
         countryCode = COUNTRY,
         decimalLongitude = DEG_LON,
         decimalLatitude = DEG_LAT,
         coordinateUncertaintyInMeters = BIAS,
         geodeticDatum = "WGS84",
         georeferenceProtocol = "Hand-held GPS receiver",
         georeferenceRemarks = "In cases where uncertainty is below 3 m, averaging function used to estimate coordinates",
         locality = sub(LOCALITY, patt = "�)", repl =  "°", fixed = T) 
         )

eventcore %>% 
  select(contains("event"), contains("decimal"), countryCode, locationID, geodeticDatum) %>%
  glimpse()

eventcore %>% 
  select(contains("event"), contains("decimal"), countryCode, locationID, geodeticDatum) %>%
  summary()

### 1.3.3. Site locality: sampleSizeValue and sampleSizeUnit - diff for each eventID/level ----
# https://docs.gbif.org/survey-monitoring-quick-start/en/#survey-site-area-terms

eventcore %<>%
  mutate(sampleSizeValue = SURF_AREA,
         sampleSizeUnit = "square metre")
eventcore
eventcore %>% select(contains("event"), contains("sample"))

### 1.3.4. Vegetation cover: see Releve extension ----
##  1.4 Survey date and time ----
eventcore %<>%
  mutate(eventDate = as.Date(DATE, format = "%Y%m%d") %>% format("%Y-%m-%d"))
# Sampling protocol - the same at all levels?

##  1.5 Sampling event protocol ----

# https://docs.gbif.org/survey-monitoring-quick-start/en/#what-is-sampling-protocol
# NOTE: Sampling protocol terms should be populated at every event level possible as inheritance 
# in either direction should not be assumed or inferred between event levels.

### 1.5.1  Event type - different at each hierarchical level (PhD project, plot, nested-plots) ----
# https://docs.gbif.org/survey-monitoring-quick-start/en/#event-type 
eventcore %<>%
  mutate(eventType = case_when(
    is.na(parentEventID) ~ "PhD project",
    parentEventID == "SP" ~ "Vegetation survey",
    .default = "Vegetation subsurvey (nested plot)"),
    samplingProtocol = case_when( # At all levels
      !is.na(parentEventID) ~ "Vegetation plot (releve)")
    ) 

eventcore %>%
  select(eventID, parentEventID,eventType)

eventcore %>%
  count(eventType)

### 1.5.2 Sampling protocol - different at each hierarchical level (PhD project, plot, nested-plots) ----
eventcore %<>%
  mutate(
    samplingProtocol = case_when( # At all levels
      COVERSCALE == "00" ~ "Vegetation plot (Relevé)",
      COVERSCALE == "04" ~ "Vascular plant survey (presence only)"
      )
  ) 
eventcore %>%
  select(contains("event"), samplingProtocol)

eventcore %>%
  group_by(eventType, samplingProtocol) %>%
  count()

### 1.5.3 Material sample - none for eventcore, see Humboldt ----
### 1.5.4 Vouchers - none for eventcore, see humoldt ----
### 1.5.5 Least specific target category quantity inclusive - none for eventcore, see humoldt ----

### 1.5.6 Data generalizations & information withheld - none ----

### 1.5.7 Verbatim fields - fieldNotes and eventRemarks ----
eventcore %<>%
  mutate(
    fieldNotes = NA, # Could be checked in the notebooks and updated accordingly on a a later stage
    eventRemarks = MANAGEMENT,
    eventRemarks = case_match(eventRemarks, 
                          'grazed+burn' ~ 'grazed and burned', 
                          'mowed+burne' ~ 'mowed and burned',
                          .default = eventRemarks
      )
    )

eventcore %>% count(eventRemarks)
eventcore %>%
  select(contains("event"), fieldNotes, eventRemarks)

##  1.6 Scope and completeness ----
# Source: https://docs.gbif.org/survey-monitoring-quick-start/en/#scope-and-completeness

# NOTE: Sampling protocol terms should be populated at every event level possible as inheritance 
# in either direction should not be assumed or inferred between event levels.
# 1.6.1. Verbatim scope - none for eventcore, see humboldt ----
# 1.6.2. Taxonomic scope - identifiedBy recommended for eventcore but I will add it the occurrence ----
# 1.6.3. Organismal scope - none for eventcore, see humboldt ----
# 1.6.3. Bycatch scope - none for eventcore, see humboldt ----
# 1.6.4. Habitat scope - none for eventcore, see humboldt ----
## 1.7. Sampling Effort ----
eventcore %<>%
  rowwise() %>%
  mutate(
    nr_observers = stringr::str_count(AUTHORS, ',') + 1,
    recordedBy = sub(AUTHORS, 
                     patt = "(.+)(\\,\\s?|\\s)(Salza Todorova)", 
                     repl = "\\3\\,\\1"),
    recordedBy = sub(recordedBy, patt = "Chytry", repl = "Chytrý"),
    recordedBy = sub(recordedBy, patt = "Jir�", repl = "Jiří"),
    recordedBy = sub(recordedBy, patt = "Axmanov�", repl = "Axmanová"), 
    recordedBy = sub(recordedBy, patt = "Merunkov�", repl = "Merunková"), 
        recordedBy = sub(recordedBy, patt = "M.rio Ducho.{1}", repl = "Mário Duchoň"),#ň
    recordedBy = sub(recordedBy, patt = "Merunkov�", repl = "Merunková"),
    recordedBy = gsub(recordedBy, patt = "\\,$?", repl = ""),
        recordedBy = gsub(recordedBy, patt = "\\,\\s?", repl = "|")
    # recordedBy = recordedBy %>% str_squish()
    ) %>%
  ungroup() %>%
  mutate(
    samplingEffort = case_when(
      SURF_AREA == 1 ~ paste(nr_observers, 
                             'observers sampled until no new species found in the plot for >=1 min'),
      SURF_AREA == 16 ~ paste(nr_observers, 
                             'observers sampled until no new species found in the plot for >=3 min'),
      SURF_AREA == 100 ~ paste(nr_observers, 
                             'observers sampled until no new species found in the plot for >=5 min')
      )
    ) 

eventcore %>%
  filter(!is.na(parentEventID)) %>%
  glimpse()

## 1.8. Final selection and EXPORT ----
eventcore %>%
  select(any_of(DwC_Event_terms$term)) %>%
  write_delim("Palpurina_PhD_eventcore.txt", 
              delim = "\t", 
              quote = "all", 
              na = "")



# -- GBIF Relevé extension ----

# Read the XML file
xml_doc <- read_xml("https://rs.gbif.org/extension/gbif/1.0/releve_2016-05-10.xml")

# Find namespace (if any)
ns <- xml_ns(xml_doc)

# Locate <property> elements (considering namespaces)
properties <- xml_find_all(xml_doc, ".//d1:property", ns)

# Convert the extracted data into a dataframe
releve_ext_terms <- tibble(
  term = xml_attr(properties, "name"),
  type = xml_attr(properties, "type"),
  descr = xml_attr(properties, "description"),
  required= xml_attr(properties, "required")
)

# View the data frame
print(releve_ext_terms)

eventcore %>% 
  select(contains("clas"), HABITAT) %>% 
  distinct(PHYTCLASS4, HABITAT) %>% 
  arrange(PHYTCLASS4)

eventcore %<>% 
  mutate(syntaxonName = case_when(
    grepl(PHYTCLASS4, patt = "^FES") ~ "Festuco-Brometea", 
    grepl(PHYTCLASS4, patt = "GER+FES", fixed = T) ~ "Trifolio-Geranietea sanguinei",
    grepl(PHYTCLASS4, patt = "MOL+FES", fixed = T) ~ "Molinio-Arrhenatheretea",
    grepl(PHYTCLASS4, patt = "^TRA") ~ "Stipo-Trachynietea distachyae")
  )

eventcore %<>% 
  mutate(syntaxonName = replace(syntaxonName,
                                HABITAT_SH == "MedGrass",
                                "Stipo-Trachynietea distachyae")
  )

eventcore %>% 
  select(PHYTCLASS4,syntaxonName, HABITAT) %>% 
  distinct(PHYTCLASS4,syntaxonName, HABITAT) %>% 
  arrange(PHYTCLASS4, syntaxonName) 

releve_ext <- 
  eventcore %>%
  filter(parentEventID == "SP") %>%
  mutate(project = "Fieldwork for the PhD project of Salza Palpurina (Masaryk Uni)",
         coverCryptogamsInPercentage = NA
  ) %>%
  select(eventID,
         FIELD_NO,
         project,
         syntaxonName,
         aspect = EXPOSITION,
         inclinationInDegrees = INCLINATIO,
         coverTotalInPercentage = COV_TOTAL,
         coverTreesInPercentage = COV_TREES,
         coverShrubsInPercentage = COV_TREES,
         coverHerbsInPercentage = COV_HERBS,
         coverCryptogamsInPercentage,
         coverMossesInPercentage = COV_MOSSES,
         coverLichensInPercentage = COV_LICHEN,
         coverAlgaeInPercentage = COV_ALGAE,
         coverLitterInPercentage = COV_LITTER,
         coverWaterInPercentage = COV_WATER,
         coverRockInPercentage = COV_ROCK,
         treeLayerHeightInMeters = TREE_HIGH,
         shrubLayerHeightInMeters = SHRUB_MEAN,
         herbLayerHeightInCentimeters = HERB_MEAN,
         mossesIdentified = MOSS_IDENT,
         lichensIdentified = LICH_IDENT
  )
releve_ext %>% summary()


releve_ext %<>%
  mutate(across(
    .cols = where(is.numeric) & matches("cover"),
    ~ replace(.x, .x == -1, NA)
  )) 
releve_ext %>% summary()


releve_ext %>% glimpse()


convert_aspect <- function(degrees) {
  ifelse(is.na(degrees), NA,
         dplyr::case_when(
           (degrees >= 337.5 | degrees < 22.5)  ~ "N",
           degrees >= 22.5  & degrees < 67.5    ~ "NE",
           degrees >= 67.5  & degrees < 112.5   ~ "E",
           degrees >= 112.5 & degrees < 157.5   ~ "SE",
           degrees >= 157.5 & degrees < 202.5   ~ "S",
           degrees >= 202.5 & degrees < 247.5   ~ "SW",
           degrees >= 247.5 & degrees < 292.5   ~ "W",
           degrees >= 292.5 & degrees < 337.5   ~ "NW",
           TRUE ~ NA_character_
         )
  )
}

releve_ext %<>%
  mutate(aspect= replace(aspect, aspect == "#N/", ""),
         aspect = convert_aspect(as.numeric(aspect))
         )


releve_ext %>%
  select(any_of(releve_ext_terms$term)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()
  
releve_ext %>%
  select(any_of(releve_ext_terms$term)) %>%
  write_delim("Palpurina_PhD_releve_header.txt", 
              delim = "\t", 
              quote = "all", 
              na = "")

# -- Darwin CORE Occurrence ----

# Read the XML file
xml_doc <- read_xml("https://rs.gbif.org/core/dwc_occurrence_2024-02-23.xml")

# Find namespace (if any)
ns <- xml_ns(xml_doc)

# Locate <property> elements (considering namespaces)
properties <- xml_find_all(xml_doc, ".//d1:property", ns)

# Convert the extracted data into a dataframe
occurrence_core_terms <- tibble(
  term = xml_attr(properties, "name"),
  type = xml_attr(properties, "type"),
  descr = xml_attr(properties, "description"),
  required= xml_attr(properties, "required")
)

# View the data frame
print(occurrence_core_terms, n = nrow(occurrence_core_terms))

occurrence_core_terms %>%
  filter(required == "true")

# Add species name to the abundance table
taxon_checklist %>%
  filter(duplicated(SPECIES_NR)) # There should be no duplicates!!!

occurrences <- abund %>% as_tibble()

occurrences %<>%
  left_join(taxon_checklist %>% select(SPECIES_NR, verbatimName = ABBREVIAT), by = "SPECIES_NR")
occurrences

# Clean them  back in the dataset
occurrences %<>% 
  mutate(verbatimName = sub(verbatimName, patt = "\\sspecies$", repl =""),
         verbatimName = sub(verbatimName, patt = "\\ssp\\.", repl =""),
         verbatimName = sub(verbatimName, patt = "\\sx\\s", repl =" × ")
         )

occurrences %>% 
  filter(grepl(verbatimName, patt = "\\sspecies$"))

# Extract onyl names from the current dataset
name_list <- occurrences$verbatimName %>% unique()
name_list %>% length()

# Lookup names in GBIF BB
gbif_bb_match <- name_backbone_checklist(name_list)
gbif_bb_match %>% dim()

# Check which did not get a kingdom
no_match <- 
  gbif_bb_match %>%
  filter(is.na(kingdom)) %>%
  select(verbatim_name)
no_match

new_match <- name_backbone_checklist(name_data = no_match$verbatim_name, 
                                     kingdom = c(rep("Plantae", 12), rep("Plantae", 3), "Fungi"))

new_match %>%
  filter(is.na(kingdom)) %>%
  select(verbatim_name)

gbif_bb_match %<>%
  filter(!is.na(kingdom)) %>%
  bind_rows(new_match)

# Add taxonomic info to the occurrence dataset
occurrences %<>%
  left_join(gbif_bb_match, by = c('verbatimName' = 'verbatim_name'))

occurrences %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()

# Add other relevant terms:
occurrences %<>%
  left_join(eventcore %>% select(RELEVE_NR,eventID, 
                                 recordedBy, # assumed to be the determiners as well
                                 parentEventID, 
                                 SURF_AREA))
occurrences

occurrences %<>%
  mutate(occurrenceID = paste0(eventID, "_TVId", SPECIES_NR),
         basisOfRecord = "HumanObservation",
         occurrenceStatus = "present", # STRONGLY RECOMMENDED
         organismQuantity = case_when(
           parentEventID == "SP" ~ COVER_CODE,
           .default = NA),
         organismQuantityType = case_when(
           parentEventID == "SP" ~ "% cover",
           .default = NA)
  )
occurrences %<>% select(-parentEventID)

# CHECK occurrence ID is unique!
occurrences %>%
  filter(duplicated(occurrenceID)) %>%
  use_series(occurrenceID)

# Keep only unique occurrenceIds
occurrences %<>%
  distinct(.keep_all = TRUE, pick(occurrenceID))


# Identified by:
occurrences %<>%
  mutate(identifiedBy = recordedBy,
         identifiedBy = replace(identifiedBy, kingdom == 'fungi', "Alice Dingova"),
         identifiedBy = replace(identifiedBy, grepl(scientificName, patt = "Thymus\\s."), "Jaroslav Čáp"),
         identifiedBy = replace(identifiedBy, grepl(scientificName, patt = "Valerianella\\s."), "Zdenek Kaplan"),
         identifiedBy = replace(identifiedBy, grepl(scientificName, patt = "Alyssum\\s."),"Stanislav Španiel"),
         identifiedBy = replace(identifiedBy, grepl(scientificName, patt = "Festuca\\s."), "Petr Šmarda")
  )

# Final selection
occurrences %>%
  distinct(.keep_all = TRUE, pick(occurrenceID)) %>%
  select(any_of(occurrence_core_terms$term)) %>%
  write_delim("Palpurina_PhD_occurrences.txt", 
              delim = "\t", 
              quote = "all", 
              na = "")

# 2. Humboldt extension (TO BE FINISHED) ----
# I find the following document very useful: https://eco.tdwg.org/hierarchy/
##  1.1 Import the extension terms ----
# Read the XML file
xml_doc <- read_xml("https://rs.gbif.org/extension/eco/Humboldt_2024-04-16.xml")

# Find namespace (if any)
ns <- xml_ns(xml_doc)
ns

# Locate <property> elements (considering namespaces)
properties <- xml_find_all(xml_doc, ".//d1:property", ns)

# Convert the extracted data into a dataframe
DwC_Humboldt <- tibble(
  term = xml_attr(properties, "name"),
  type = xml_attr(properties, "type"),
  required= xml_attr(properties, "required"),
  descr = xml_attr(properties, "description")
)
print(DwC_Humboldt, n = nrow(DwC_Humboldt))

# DwC_Humboldt %<>%
#   mutate(required = replace(required, 
#                             term %in% c("eventID", 
#                                         "eventDate", 
#                                         "samplingProtocol",
#                                         "sampleSizeValue", 
#                                         "sampleSizeUnit"), 
#                             "true"))

# DwC_Event_terms_strongly_recommended <- c("countryCode", 
#                                           "parentEventID", 
#                                           "samplingEffort",
#                                           "locationID",
#                                           "decimalLatitude", 
#                                           "decimalLongitude",
#                                           "geodeticDatum",
#                                           "coordinateUncertaintyInMeters",
#                                           "footprintWKT")

# NOTE: No terms seems required here, but the required terms are listed here:
# https://www.gbif.org/data-quality-requirements-sampling-events#dcEventID

DwC_Humboldt %>% 
  filter(required == "true")


## 2.1. Survey sampling design and event hierarchy - eventID, parentEventID and siteCount ----
humboldt <- 
  eventcore %>% 
  group_by(FIELD_NO) %>%
  add_count(name = "nr_nested_plots") %>%
  mutate(plotSizes = toString(SURF_AREA)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(nestedPlotSeriesDescr = 
           paste("a series of",nr_nested_plots, 
                 "nested plots of sizes (in sqm):", 
                 plotSizes)
         ) %>%
  ungroup()

# # I will only summarize info for the largest plot and the PhD project-level
# humboldt %<>% 
#   filter(eventID == "SP" | grepl(eventID, patt = "_100")) 

humboldt %>%
  count(nestedPlotSeriesDescr)

# Hierarchy terms - siteNestingDescription
humboldt %<>%
  #filter(parentEventID == "SP" | is.na(parentEventID))
  rowwise() %>%
  mutate(
    siteNestingDescription = case_when(
      nr_nested_plots == 1 ~ "a single 10×10 m2",
      nr_nested_plots > 1 && parentEventID == "SP" ~
        paste("largest plot from",nestedPlotSeriesDescr ),
      parentEventID != "SP" && SURF_AREA == 1 ~
        paste("smallest plot from",nestedPlotSeriesDescr ),
      parentEventID != "SP" && SURF_AREA != 1 ~
        paste("mid-sized plot from",nestedPlotSeriesDescr )
      )
    ) %>%
  ungroup()

humboldt %<>%
  mutate(siteNestingDescription = 
           replace(siteNestingDescription,
                   nr_nested_plots == 1 ,
                   "a single 10×10 m2  plot"),
         siteNestingDescription = 
           replace(siteNestingDescription,
                   eventID == "SP",
                   paste(header %>% filter(parentEventID == "SP") %>% nrow(), 
                         "sites of a single 10×10 m2 plot or a 10×10 m2 plot, with a nested square plot of size 1 and/or 16 sq.m.")
           )
    
    )
              
humboldt %>% count(siteNestingDescription)

# Hierarchy terms - siteCount
humboldt %<>%
  mutate(
    siteCount = case_when(
      eventID == "SP" ~ n_distinct(header$FIELD_NO),
      .default = 1)
    )

humboldt %>% count(siteCount)

humboldt %>% select(ends_with('ID'), siteNestingDescription, siteCount)

## 2.2 Survey event site ----
### 2.2.1 Site description: verbatimSiteNames and verbatimSiteDescriptions -----
humboldt %<>%
  mutate(
    verbatimSiteNames = SETTLEMENT,
    verbatimSiteNames = replace(verbatimSiteNames,
                                eventID == "SP",
                                "Bulgaria and SE Romania (Dobrogea)"
                                )) %>%
  rowwise() %>%
  mutate(geomorph = case_when(
    CONV_RIDGE == 1 ~ 'on a convex ridge',
    FLAT_RIDGE == 1 ~ 'on a flat ridge',
    GENTLE_SLO == 1 ~ 'on a gentle slope',
    FLAT_BOTTM == 1 ~ 'in a flat bottom',
    DEPRESSION == 1 ~ 'in a depression',
    MOIST_DEPR == 1 ~ 'in a moist ridge',
    .default = ""),
    soil_depth_verbatim = case_when(
      SOIL_DEPTH == 30 ~ 'on deep soil (>= 30 cm)',
      SOIL_DEPTH <= 10 ~ 'on very shallow soil (<= 10 cm)',
      .default = 'on shallow soil (11-20 cm)'),
    verbatimSiteDescriptions = case_when(
      eventID == "SP" ~ NA,
      .default = paste(HABITAT, eventRemarks, soil_depth_verbatim, geomorph, 
                                     sep = ", ")),
    
    ) %>%
  ungroup() %>%
  mutate(verbatimSiteDescriptions = sub(verbatimSiteDescriptions, patt = "(.+)\\s\\,$", repl = "\\1"))

humboldt %>% count(verbatimSiteNames)
humboldt %>% count(verbatimSiteDescriptions)


### 2.2.2 Site locality: Survey site area terms ----
humboldt %<>%
  mutate(
    geospatialScopeAreaValue = 100, # at the paren
    geospatialScopeAreaUnit = "m2",
    geospatialScopeAreaValue = replace(geospatialScopeAreaValue,
                                      eventID == "SP",
                                      "110000"),
    geospatialScopeAreaUnit = replace(geospatialScopeAreaUnit,
                                      eventID == "SP",
                                      "km"),
    totalAreaSampledValue = 100,
    totalAreaSampledUnit= "m2") %>%
  rowwise() %>%
  mutate(
    totalAreaSampledValue = replace(totalAreaSampledValue,
                                    eventID == "SP",
                                    siteCount*totalAreaSampledValue)
    ) %>%
  ungroup()

humboldt %>% select(eventID, siteCount, contains('area',ignore.case = T)) %>%
  glimpse()

### 2.2.3 Vegetation: isVegetationCoverReported ----

humboldt %<>%
  mutate(isVegetationCoverReported = if_any(any_of(c("COV_TOTAL", "COV_HERBS")), ~ .x != -1)
         ) 
humboldt %>%
  select(eventID, isVegetationCoverReported, contains('cov')) %>%
  filter(isVegetationCoverReported == TRUE & !grepl(eventID, patt = "_100"))

## 2.3 Survey date and time: not reported ----
## 2.4 Sampling protocol ----
### 2.4.1 Terms associated with eventType - N/A ----
### 2.4.2 Sampling protocol: protocolNames, protocolDescriptions and protocolReferences ----

humboldt %<>%
  mutate(protocolNames = eventcore$samplingProtocol,
         protocolReferences = case_when(
           eventID == "SP" ~ "Palpurina, S., Chytrý, M., Tzonev, R., Danihelka, J., Axmanová, I., Merunková, K., Duchoň, M. & Karakiev, T. (2015) Patterns of fine-scale plant species richness in dry grasslands across the eastern Balkan Peninsula. Acta Oecologica-International Journal of Ecology, 63, 36–46.",
           .default = NA),
         protocolDescriptions = case_when(
           eventID == "SP" ~ "Field work was carried in Bulgaria 2020 and 2012. Study sites were selected based on the literature and local expert suggestions with the aim of covering all major types of dry grasslands (petrophytic steppes, loess steppes, and semi-dry grasslands) in Bulgaria. Dry grasslands were selected based on the presence of species characteristic of the phytosociological class Festuco-Brometea. However, some pseudo-steppes from the class Stipo-Trachynietea distachyae were also sampled (n =9) and are here provided but were not analysed in Palpurina et al. (2015). Abandoned, recently strongly disturbed, or intensively grazed grasslands were avoided. However, as most of the loess steppes we visited were burnt in the previous year, we sampled some post-fire sites there. At each dry grassland site, 10×10 m2 vegetation plots were sampled in the centre of physiognomically uniform vegetation stands with homogeneous abiotic conditions. If a site was considerably topographically or edaphically variable, and this was reflected in the grassland structure and composition, we sampled more than one vegetation plot per site. This resulted in a total of 181 vegetation plots. In order to sample dry grasslands at the peak of their growing season, field work was carried out each year from the end of May till the end of June, and in the first week of July at higher altitudes. Within each 100-m2 plot, all vascular plant species (root presence) as well terricolous bryophytes and lichens were recorded together, with their relative abundance estimated as a percentage cover (vertical project of their canopy). Total vegetation cover and that of separate vegation layers (shrub, herb, cryptogam), cover of bare soil, rocks/stone and litter was also visually estimated as percentage cover. Since 2011 vascular plant were recorded (presence only) in an additional nested plot of size 1×1 m^2; in 2012 it was nested in an additional 4×4 m^2. Specimens that were not possible to identify with certainty in the field were sampled and subsequently identified in the laboratory. Lichenized fungi were determined by Alice Dingova (Slovak Academy of Sciences). Specimens of vascular plants were deposited in BRNU, of lichens in SOMF, and of bryophytes in SOM. For each plot, geographical coordinates and altitude were measured with a GPS device. Slope(°) and aspect (°) were measured by a clinometer and compass. The cover of bare rock and gravel was estimated visually in percentages. Soil depth was measured within each plot at 10 systematically placed points with a 30-cm metal rod. The mean soil depth was considered as a surrogate for moisture availability, and the coefficient of variation (CV) of soil depth was calculated as a measure of soil depth heterogeneity. In the analyses, soils deeper than 30 cm were considered as 30 cm deep. Soil samples were taken from each plot at five points (in the centre and the four corners) at a depth of 3e10 cm, and subsequently air-dried. Soil pH was measured separately for each soil sample after an 8e10 h extraction in distilled water (2:5 soil:water weight ratio). The mean value of soil pH per plot and its coefficient of variation (CV) were used for analyses, the latter being considered as a measure of soil pH heterogeneity. Nutrients were measured in a mixture of the five soil samples following standard protocols. Total nitrogen (Ntot) was determined using the Kjehldahl method. Plantavailable phosphorus (P), potassium (K), and calcium (Ca) were extracted by the Mehlich III method. The P content was determined by spectrophotometer (Spekol 210, Carl Zeiss, Jena, Germany), andK and Ca contents by atomic absorption spectrophotometer (AAS 933 Plus, GBC Scientific Equipment, Melbourne, Australia). Total organic carbon (Corg) was determined by loss on ignition at 550°C for 16 h. The C/N ratio was calculated as Corg/Ntot. Management of the dry grasslands was assessed in the field, and assigned to the following four categories: grazed (parts of plants missing, animal dung present, livestock present at the site or nearby), mown (no or little litter accumulated), burned (charcoal present, no litter), and abandoned (litter accumulated).",
           .default = NA)
  )


humboldt %>% select(eventID, contains('protocol'))

### 2.4.3 Material samples: hasMaterialSamples = FALSE ----
humboldt %<>%
  mutate(hasMaterialSamples = FALSE)

### 2.4.4 Vouchers: hasVouchers, voucherInstitutions ----
humboldt %<>%
  mutate(hasVouchers = case_when(
    parentEventID == "SP" ~ TRUE,
    .default = FALSE),
    voucherInstitutions = ifelse(MOSS_IDENT == 'N',
                                 ifelse(LICH_IDENT == 'N', "BRNU", "BRNU|SOMF"),
                                 ifelse(LICH_IDENT == 'N', "BRNU|SOM", "BRNU|SOM|SOMF"))
    )

humboldt %>% select(eventID, contains('voucher'))

### 2.4.5 Least specific target category quantity inclusive:  ----
humboldt %<>%
  mutate(isLeastSpecificTargetCategoryQuantityInclusive = TRUE)

### 2.4.6 Data generalizations & information withheld: NONE  ----


### 2.5 Scope and completeness: .....  ----

humboldt %<>% 
  mutate(verbatimTargetScope = "Vascular plants, terricolous lichenized fungi and bryophytes",
         targetTaxonomicScope = "Plantae | Ascomycota",
         isTaxonomicScopeFullyReported= case_when(
           eventID == "SP" ~ "reportedIncomplete",
           .default = "FullyReported"),
         taxonCompletenessReported = case_when(
           eventID == "SP" ~ FALSE,
           .default = 'based on sampling effort'),
         taxonCompletenessProtocols = case_when(
           eventID == "SP" ~"",
           .default = 'based on sampling effort'),
         targetHabitatScope = 'dry to semi-dry grasslnads',
         excludedHabitatScope = "wet grasslands"
  ) 

### 2.6 Sampling effort ---
humboldt %<>% 
  mutate(isSamplingEffortReported = case_when(
    eventID == "SP" ~ FALSE,
    .default = TRUE),
    samplingEffortValue = eventcore$nr_observers,
    samplingEffortUnit= "observers",
    samplingEffortProtocol = eventcore$samplingEffort,
    samplingPerformedBy = eventcore$recordedBy
    )

humboldt %>% glimpse()

## 2.X. Final selection and EXPORT ----
  humboldt %>%
    select(eventID,
           any_of(DwC_Humboldt$term)) %>%
  # glimpse() %>%
  write_delim("Palpurina_PhD_humboldt.txt", 
                delim = "\t", 
                quote = "all", 
                na = "")
  
  
  
  
  
