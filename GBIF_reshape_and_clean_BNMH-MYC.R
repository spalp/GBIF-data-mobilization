library(tidyverse)
library(sf)
library(leaflet)

# Options
old <- options(pillar.sigfig = 7)
tibble(x = 123.4567)

# # Restore old options, see also rlang::local_options() for a more elegant way
# options(old)

setwd('G:/My Drive/6.Projects/GBIF/Data mobilization/NMNHS/Herbarium BNHM')
getwd()


occurrence <- read_csv('query_results_2024-03-09T10 12 34.262300.csv', 
                col_names = T)
occurrence %>% glimpse()


occurrence %>%
  mutate(
    modified = paste0(`Collection Object Modified Date (Year)`, "-", 
                      `Collection Object Modified Date (Month)`, "-",
                      `Collection Object Modified Date (Day)`),
    dateIdentified = paste0(`Determined Date (Year)`, "-", 
                            `Determined Date (Month)`, "-",
                            `Determined Date (Day)`),
    eventDate_comb = paste0(`Start Date (Year)`, "-", 
                            `Start Date (Month)`, "-", 
                            `Start Date (Day)`),
    basisOfRecord = "PreservedSpecimen",
    occurrenceStatus = 'present',
    language = 'en',
    `Type Status` = replace(`Type Status`, `Type Status` == "|null|", "none"),
    `Taxon Source` = sub(x = `Taxon Source`, patt = '/', repl = "; ", fixed = T)
    ) %>%
  select(occurrenceID = `Collection Object GUID`,
         type = `Collection Type`,
         modified,
         language,
         license  = `Institution Copyright`,
         rightsHolder = Institution,
         accessRights = `Terms Of Use`,
         institutionID = `Institution URL`,
         collectionID = `Division URL`,
         institutionCode = `Institution Code`,
         collectionCode = `Collection Code`,
         datasetName = `Alt Name`,
         ownerInstitutionCode = `Institution Code`,
         basisOfRecord,
         catalogNumber = `Catalog Number`,
         otherCatalogNumbers = `Alt Cat Number`,
         recordedBy = Collectors,
         # lifeStage = `Collection Object Phenology`,
         -`Collection Object Phenology`, # for lichents
         occurrenceStatus = occurrenceStatus,
         preparations = Preparations,
         materialEntityRemarks = `Collection Object Specimen\nDescription`,
         eventDate = eventDate_comb,
         verbatimEventDate  = `Verbatim Date`,
         year = `Start Date (Year)`,
         month = `Start Date (Month)`,
         day = `Start Date (Day)`,
         habitat = `Locality and\n Habitat Notes`,
         # eventRemarks = `Collectors Remarks`, # Creates duplicites, DO NOT EXPORT
         locationID  = `Locality ID`,
         continent = Continent,
         country = Country,
         stateProvince = `Province/District`,
         # stateProvince = `County`, # For lichens
         municipality = Municipality,
         locality = Locality,
         minimumElevationInMeters = `Min Elevation`,
         maximumElevationInMeters  = `Max Elevation`,
         decimalLatitude = Latitude1,
         decimalLongitude = Longitude1,
         geodeticDatum = `Locality Datum`,
         coordinateUncertaintyInMeters = `Max Uncertainty Est`,
         georeferencedBy = `Geo Ref Det By`,
         georeferencedDate = `Geo Ref Det Date`,
         georeferenceProtocol = `Lat/Long Method`,
         typeStatus = `Type Status`,
         identifiedBy = Determiner,
         dateIdentified,
         kingdom = Kingdom,
         # phylum = Phylum,
         division = Division,
         class  = Class,
         family = Family,
         genus = Genus,
         specificEpithet = Species,
         #  identificationReferences 
         identificationQualifier = `Determinations Qualifier`,
         scientificNameID = `Taxon Source`,
         acceptedNameUsageID = `Preferred Taxon Source`,
         scientificName = Taxon,
         acceptedNameUsage = `Preferred Taxon`,
         scientificNameAuthorship =  `Taxon Author`,
         `Start Date`,
         ) %>%
  mutate(across(contains("Date"), ~gsub(.x, patt = ".", repl = "-", fixed = T))) %>% 
  mutate(across(c(modified, dateIdentified, eventDate),
                ~as.Date.character(.x, format = "%Y-%m-%d")),
         georeferencedDate = as.Date.character(georeferencedDate, format = "%d-%m-%Y")) %>% 
  rowwise() %>%
  mutate(`Start Date` = replace(`Start Date`,
                             grepl(`Start Date`, patt = "^[[:alnum:]]{4}$"),
                             as.Date.character(`Start Date`, format = "%Y")),
         `Start Date` = replace(`Start Date`,
                             grepl(`Start Date`, patt = "^[[:alnum:]]{2}\\.[[:alnum:]]{4}$"),
                             as.Date.character(`Start Date`, format = "%m-%Y")),
         `Start Date` = replace(`Start Date`,
                             grepl(`Start Date`, patt = "^[[:alnum:]]{2}\\.[[:alnum:]]{2}\\.[[:alnum:]]{4}$"),
                             as.Date.character(`Start Date`, format = "%d-%m-%Y")),
         dateIdentified  = replace(dateIdentified,
                                   grepl(dateIdentified, patt = "^[[:alnum:]]{4}$"),
                                   as.Date.character(dateIdentified, format = "%Y")),
         dateIdentified = replace(dateIdentified, grepl(dateIdentified, patt = "^[[:alnum:]]{2}\\.[[:alnum:]]{4}$"),
                             as.Date.character(dateIdentified, format = "%m-%Y")),
         dateIdentified = replace(dateIdentified,
                                  grepl(dateIdentified,
                                        patt = "^[[:alnum:]]{2}\\.[[:alnum:]]{2}\\.[[:alnum:]]{4}$"),
                                  as.Date.character(dateIdentified, format = "%d-%m-%Y"))
         ) %>%
  ungroup() %>%
  # glimpse()
  write_delim("MYC_query_results_2024-03-09T10 12 34.262300.txt", delim = '\t', na = "")

