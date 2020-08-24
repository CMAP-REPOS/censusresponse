### Loading Packages and APIs  --------------------


#install.packages("tidyverse")
# install.packages("censusapi")
# install.packages("tidycensus")

library(tidyverse)
library(censusapi)
library(tidycensus)


### Loading Census API Key ----------

# Add your own census API key to CURRENT .Renviron for getCensus
Sys.setenv(CENSUS_KEY="YOUR KEY HERE")
readRenviron("./.Renviron")
Sys.getenv("CENSUS_KEY")

# # Add key to .Renviron for tidycensus 
# # Run only once per machine.
census_api_key("YOUR KEY HERE")

### Geographic identifiers --------------------

# CMAP area, county FIPS codes
cmap_counties <- c("031", "043", "089", "093", "097", "111", "197")

### Download response rate data ------------------------

# Variables:  NAME: City/town/locality name
#             GEO_ID: Combined codes for the reference geography
#             CRRALL: Cumulative Self-Response Rate - Overall
#             CRRINT: Cumulative Self-Response Rate - Internet
#             RESP_DATE: Most recent data cutoff for responses received, 
#                          point in time response rates are calculated for
#             PLACE: Place
#             FSRR2010: Final Self Response Rate 2010

### download demographic info ---------------------

## function to pull ACS data for relevant variables at the tract level
demo_puller <- function(year,survey,stateFIPS,countyFIPS,ACSvariables) {
  data <- get_acs(geography = "tract", 
                  variables = ACSvariables, 
                  cache_table = TRUE, 
                  year = year, 
                  state = stateFIPS, 
                  county = countyFIPS, 
                  survey = survey,
                  output = "wide")
  return(data)
}

## Variables for 2010 ACS
ACSvariables2010 <- c("B01001_001",    # total population
                      "B19013_001",    # median HH income
                      "B11005_001",    # number of households
                      "B25001_001",    # number of housing units
                      "DP05_0066P",    # percent hispanic or latino, any race
                      "DP05_0072P",    # percent white, non-hispanic
                      "DP05_0073P",    # percent black, non-hispanic
                      "DP05_0075P"     # percent asian, non-hispanic
)

## Variables for 2018 ACS
ACSvariables2018 <- c("B01001_001",    # total population
                      "B19013_001",    # median HH income
                      "B11005_001",    # number of households
                      "B25001_001",    # number of housing units
                      "S1701_C03_001", # percent below poverty level
                      "S1602_C04_001", # percent limited english speaking
                      "DP05_0071P",    # percent hispanic or latino, any race
                      "DP05_0077P",    # percent white, non-hispanic
                      "DP05_0078P",    # percent black, non-hispanic
                      "DP05_0080P"     # percent asian, non-hispanic
)

## clean demographic data

# Create functions to clean 2010 and 2018 data
demogs_tract_2010_cleaner <- function(data) {
  output <- data %>% 
  # drop MOEs and rename
  select(GEOID, 
         NAME,
         popul = B01001_001E,
         hhold = B11005_001E,
         hhinc = B19013_001E,
         hunit = B25001_001E,
         hispa = DP05_0066PE,
         white = DP05_0072PE,
         black = DP05_0073PE,
         asian = DP05_0075PE) %>% 
  mutate(
    # calculate 'other' race bucket
    other = round(100 - hispa - white - black - asian,
                  digits = 1),
    nonwh = round(100 - white,
                  digits = 1)) %>% 
  # separate FIPS code
  separate(GEOID, into = c("state", "county", "tract"),
           sep = c(2,5), remove = FALSE)
  return(output)
}

# same function as 2010
demogs_tract_2018_cleaner <- function(data) {
  output <- data %>% 
  select(GEOID, 
         NAME,
         popul = B01001_001E,
         hhold = B11005_001E,
         hunit = B25001_001E, 
         hhinc = B19013_001E,
         povty = S1701_C03_001E,
         lmeng = S1602_C04_001E, 
         hispa = DP05_0071PE,
         white = DP05_0077PE,
         black = DP05_0078PE,
         asian = DP05_0080PE) %>% 
  mutate(
    other = round(100 - hispa - white - black - asian,
                  digits = 1),
    nonwh = round(100 - white,
                  digits = 1)) %>% 
  separate(GEOID, into = c("state", "county", "tract"),
           sep = c(2,5), remove = FALSE)
  return(output)
}

# same function structure but used to clean "place"-based data for 2018
demogs_place_2018_cleaner <- function(data) {
  output <- data %>% 
  select(GEOID, 
         NAME,
         popul = B01001_001E,
         hhold = B11005_001E,
         hunit = B25001_001E, 
         hhinc = B19013_001E,
         povty = S1701_C03_001E,
         lmeng = S1602_C04_001E, 
         hispa = DP05_0071PE,
         white = DP05_0077PE,
         black = DP05_0078PE,
         asian = DP05_0080PE) %>% 
  mutate(
    other = round(100 - hispa - white - black - asian,
                  digits = 1),
    nonwh = round(100 - white,
                  digits = 1))
  return(output)
}


### Combine demographic and response rate data -----


## Create relationship between 2010 and 2020 tracts
# The Crosswalk file was provided by the US Census Bureau. It creates a relationship
#   between 2010 and 2020 tracts.

crosswalk <- read.csv("./sources/rr_tract_rel.txt") %>%
  # Add leading spaces for states with FIPS codes that start with 0
  mutate(GEOID10 = sprintf("%011s",as.character(GEOID10)),
         GEOID20 = sprintf("%011s",as.character(GEOID20))) %>%
  # Replace leading spaces with 0
  mutate(GEOID10 = gsub('^[ ]{1,}','0',GEOID10),
         GEOID20 = gsub('^[ ]{1,}','0',GEOID20))



########################### Full USA Data #####################################


## Pull data for all tracts in the US

# Empty data frame
response_tract_blank <- getCensus(name = "dec/responserate",
            vintage = "2020",
            vars = c("NAME", "GEO_ID", "CRRALL","CRRINT","RESP_DATE"),
            region = "tract:*",
            regionin = "state:02")

response_tract_USA <- response_tract_blank[0,]

# List of FIPS codes for US states + DC
states = unique(fips_codes$state_code)[1:51]

# Pull self-response information for all tracts in the US
for (i in  states) {
  response_tract_USA <- rbind(response_tract_USA,
                              full_join(
                                getCensus(name = "dec/responserate",
                                          vintage = "2020",
                                          vars = c("NAME",
                                                   "GEO_ID",
                                                   "CRRALL",
                                                   "CRRINT",
                                                   "RESP_DATE"),
                                          region = "tract:*",
                                          regionin = paste("state:",i)),
                                getCensus(name = "dec/responserate",
                                          vintage = "2010",
                                          vars = c("NAME", 
                                                   "GEO_ID",
                                                   "FSRR2010"),
                                          region = "tract:*",
                                          regionin = paste("state:",i)),
                                by = c("GEO_ID",
                                       "state",
                                       "county", 
                                       "tract", 
                                       "NAME")) %>% 
                                # clean up data, and calculate current difference
                                mutate(CRRALL = as.numeric(CRRALL),
                                       CRRINT = as.numeric(CRRINT),
                                       dif = CRRALL - FSRR2010,
                                       GEOID = str_replace(GEO_ID, "1400000US", "")) %>% 
                                select(GEOID, 
                                       GEO_ID, 
                                       state, 
                                       county, 
                                       tract, 
                                       NAME, 
                                       CRRALL, 
                                       CRRINT, 
                                       RESP_DATE, 
                                       FSRR2010, 
                                       dif) %>% 
                                filter(!((is.na(CRRALL) | is.na(FSRR2010))))
  )
}


# Pull ACS 5-year data for 2010 and 2018 at the tract level nationwide
demogs_tract_USA_2010 <- map_dfr(
  states, 
    ~ demo_puller(year = 2010,
                  survey = "acs5",
                  stateFIPS = .,
                  countyFIPS = NULL,
                  ACSvariables = ACSvariables2010))

demogs_tract_USA_2018 <- map_dfr(
  states, 
  ~ demo_puller(year = 2018,                   
                survey = "acs5",                 
                stateFIPS = .,
                countyFIPS = NULL,
                ACSvariables = ACSvariables2018))

# Clean data using functions defined above
demogs_tract_USA_2010_clean <- demogs_tract_2010_cleaner(demogs_tract_USA_2010)
demogs_tract_USA_2018_clean <- demogs_tract_2018_cleaner(demogs_tract_USA_2018)

# Create crosswalk file with relevant variables
crosswalkUSA <- crosswalk %>%
  # These variables represent the share of 2010 housing represented in
  #   the 2010 and 2020 tracts, respectively.
  select(GEOID10, 
         GEOID20,
         HU10PCT_T10,
         HU10PCT_T20,
         HUCURPCT_T10,
         HUCURPCT_T20)


## Interpolate 2010 and 2018 data to 2020 tracts.

# Add 2010 demographic data to the crosswalk table
consolidated_tract_USA_2010 <- full_join(crosswalkUSA, 
                                    demogs_tract_USA_2010_clean, 
                                    by = c("GEOID10" = "GEOID"))

# Add 2018 demographic data to the crosswalk table
consolidated_tract_USA_2018 <- full_join(crosswalkUSA, 
                                    demogs_tract_USA_2018_clean, 
                                    by = c("GEOID10" = "GEOID"))

# character vectors with relevant demographic variables
demographic_variables2010 <- c("hhinc","hispa","white","black","asian",
                               "other","nonwh")
demographic_variables2018 <- c(demographic_variables2010,"povty","lmeng")

# Interpolate ACS data to new tracts, using share of housing represented
#   as the weight for a weighted mean

# Interpolate demographic variables: We use the share of housing in the 2020 
#  tracts that was present in each corresponding 2010 tract as a proxy for the 
#  relative weighting of the impact of the source tract on the new tract's 
#  demographic breakdown (for variables that are an average across the 
#  population). For 2010 variables, we use 2010 housing. For 2018 variables, we
#  use "current" housing.
interpolated_tract_USA_2010 <- consolidated_tract_USA_2010 %>%
  group_by(GEOID20) %>%
  summarize_at(demographic_variables2010, 
               funs(weighted.mean(.,w=HU10PCT_T20,na.rm = TRUE)))

interpolated_tract_USA_2018 <- consolidated_tract_USA_2018 %>%
  group_by(GEOID20) %>%
  summarize_at(demographic_variables2018, 
               funs(weighted.mean(.,w=HUCURPCT_T20,na.rm = TRUE)))


# Interpolate population and households: We use the share of 2010 housing present  
#  in the portion of the 2010 tract to allocate population into new tract  
#  boundaries. Since this is calculated as a sum, and not an average, it accounts 
#  for cases where two tracts merge or one tract might be split. For 2010 
#  variables, we use 2010 housing. For 2018 variables, we use "current" housing.
interpolated_popul_USA_2010 <- consolidated_tract_USA_2010 %>%
  group_by(GEOID20) %>%
  summarize(popul = sum(popul * HU10PCT_T10/100,na.rm = TRUE),
            hhold = sum(hhold * HU10PCT_T10/100,na.rm = TRUE),
            hunit = sum(hunit * HU10PCT_T10/100,na.rm = TRUE))

interpolated_popul_USA_2018 <- consolidated_tract_USA_2018 %>%
  group_by(GEOID20) %>%
  summarize(popul = sum(popul * HUCURPCT_T10/100,na.rm = TRUE),
            hhold = sum(hhold * HUCURPCT_T10/100,na.rm = TRUE),
            hunit = sum(hunit * HUCURPCT_T10/100,na.rm = TRUE))

# Combine the interpolated data, adding suffixes for duplicate demographics
interpolated_tract_USA <- full_join(full_join(interpolated_tract_USA_2010,
                                              interpolated_popul_USA_2010,
                                              by = c("GEOID20")),
                                    full_join(interpolated_tract_USA_2018,
                                              interpolated_popul_USA_2018,
                                              by = c("GEOID20")),
                                    by=c("GEOID20"),
                                    suffix=c("10","18"))

# Threshold for racial majority
threshold <- 50

# Join demographic data to current census response rates (as of August 10)
final_tract_USA <- full_join(interpolated_tract_USA, 
                         response_tract_USA, 
                         by = c("GEOID20" = "GEOID")) %>%
  mutate(
    race_maj10 = case_when(
      hispa10 > threshold ~ "hispa",
      black10 > threshold ~ "black",
      asian10 > threshold ~ "asian",
      white10 > threshold ~ "white",
      other10 > threshold ~ "other",
      TRUE         ~ "none"
    )
  ) %>%
  mutate(
    race_maj18 = case_when(
      hispa18 > threshold ~ "hispa",
      black18 > threshold ~ "black",
      asian18 > threshold ~ "asian",
      white18 > threshold ~ "white",
      other18 > threshold ~ "other",
      TRUE         ~ "none"
    )
  ) 


## Pull list of top 20 MSAs by population
msa_list <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                    variables = c("B01001_001"), 
                    cache_table = TRUE, 
                    year = 2018,
                    survey = "acs5",
                    output = "wide") %>%
  # Sort by population (descending)
  arrange(-B01001_001E) %>%
  # Rename variables and select
  select(NAME,
         Popul = B01001_001E,
         GEOID) %>%
  # Keep top 20 by population
  slice_head(n = 20)

## Import crosswalk file for counties to MSAs (includes supplemental assignment for CMAP area counties)
county_msa_crosswalk <- read.csv("./sources/County_MSA_Crosswalk.csv") %>%
  # Add leading spaces for states with FIPS codes that start with 0
  mutate(County_GEOID = sprintf("%05s",as.character(County_GEOID))) %>%
  # Replace leading spaces with 0
  mutate(County_GEOID = gsub('^[ ]{1,}','0',County_GEOID)) %>%
  # Remove " (Metropolitan Statistical Area)"
  mutate(MSA = gsub("\\s*\\([^\\)]+\\)","",MSA)) %>%
  select(MSA_GEOID,MSA,County_GEOID,County)


# Subset data from relevant metropolitan area peers
final_tract_MSAs <- final_tract_USA %>%
  # Pull the State and County FIPS portion of the GEOID
  mutate(stateCountyFIPS = substr(GEOID20,0,5)) %>%
  # Join data with the county / MSA crosswalk file to assign data to MSAs
  left_join(.,county_msa_crosswalk, by = c("stateCountyFIPS"="County_GEOID")) %>%
  # Keep only data in the MSA list we are interested in
  filter(MSA_GEOID %in% msa_list$GEOID)

# Create subset only with CMAP region
final_tract_cmap <- final_tract_MSAs %>% filter(MSA == "CMAP Region")




### Analyze data ------------------------

## For MSAs

race_bucket_MSAs_2010 <- final_tract_MSAs %>%
  # Remove tracts with missing data in housing units (used for weighting) or census response
  filter(!is.na(FSRR2010),!is.na(hunit10)) %>%
  
  # Add values for "all" for both MSA and racial majority to enable total rows
  mutate_at(vars(c(race_maj10,MSA)), funs(as.character(.))) %>%
  bind_rows(mutate(., race_maj10 = "all")) %>%
  bind_rows(mutate(.,MSA = "all")) %>%
  group_by(race_maj10,MSA) %>%
  
  # Calculate mean participation rate, weighted by households in 2010
  summarize(mean10 = weighted.mean(FSRR2010,w=hunit10),
            n = n())


# Repeat analysis but for 2018 (using 2018 housing units as weights)
race_bucket_MSAs_2020 <- final_tract_MSAs %>%
  filter(!is.na(CRRALL),!is.na(hunit18)) %>%
  mutate_at(vars(c(race_maj18,MSA)), funs(as.character(.))) %>%
  bind_rows(mutate(., race_maj18 = "all")) %>%
  bind_rows(mutate(.,MSA = "all")) %>%
  group_by(race_maj18,MSA) %>%
  summarize(mean20 = weighted.mean(CRRALL,w=hunit18),
            n = n())



# join the two tables for a consolidated 2010 vs. 2020 comparison
race_bucket_MSAs_table <- left_join(race_bucket_MSAs_2010,
                                    race_bucket_MSAs_2020,
                                    by=c("race_maj10" = "race_maj18",
                                         "MSA"),
                                    suffix=c("10","18")) %>%
  # Calculate difference between the two means
  mutate(difference = mean20 - mean10) %>%
  # Select and rename relevant variables in desired order
  select(race = race_maj10,
         responses10 = mean10,
         responses20 = mean20,
         difference,
         n10,
         n20 = n18,
         MSA)

# Display tables by different racial composition of tracts
race_bucket_MSAs_table %>% filter(race == "all")
race_bucket_MSAs_table %>% filter(race == "hispa")
race_bucket_MSAs_table %>% filter(race == "black")
race_bucket_MSAs_table %>% filter(race == "white")
race_bucket_MSAs_table %>% filter(race == "asian")
race_bucket_MSAs_table %>% filter(race == "other")
race_bucket_MSAs_table %>% filter(race == "none")
race_bucket_MSAs_table %>% filter(MSA == "CMAP Region")

## Bar Charts for MSAs
# Figure 4: Bar chart of differential, hispanic vs. all responses
filter(race_bucket_MSAs_table, (race == "all" | race == "hispa") & MSA != "all") %>%
  mutate(
  legend = case_when(
    race == "all" ~ "Overall Difference",
    race == "hispa" ~ "Difference in Majority-Hispanic Tracts",
    race == "black" ~ "Difference in Majority-Black Tracts"
  )) %>%
  filter(!is.na(difference)) %>%
  ggplot(aes(fill=legend, y=difference, x=MSA)) +
  geom_bar(position=position_dodge2(reverse=FALSE),stat="identity",width = .7) +
  coord_flip() +
  scale_y_continuous(breaks=seq(-20,5,5))

### Export data for mapping and publication ---------------------
## Used for maps
write.csv(final_tract_cmap, file = "./output/tract_demog_responses_cmap.csv")

## Used for MSA comparison bar chart (formatted)

bar_chart_output <-
  filter(race_bucket_MSAs_table, (race == "all" | 
                                    race == "hispa" | 
                                    race == "black" | 
                                    race == "white" | 
                                    race == "none") & 
           MSA != "all" &
           MSA != "CMAP Region") %>%
  mutate(difference = difference/100) %>%
  pivot_wider(names_from = race, values_from = difference, id_cols=MSA) %>%
  arrange(all) %>%
  select(MSA,
         "Overall Change" = all,
         "Black Tract Change" = black,
         "Hispanic Tract Change" = hispa,
         "White Tract Change" = white,
         "No Majority Tract Change" = none)
    
write.csv(bar_chart_output, file = "./output/msa_chart_output.csv")

# Used for Chicago tract comparison by racial group (formatted)
chicago_race_output <-
  filter(race_bucket_MSAs_table, MSA == "CMAP Region" & race != "all") %>%
  mutate(race = case_when(
    race == "hispa" ~ "Hispanic",
    race == "white" ~ "White",
    race == "black" ~ "Black",
    race == "none" ~ "No Majority",
    race == "other" ~ "Other",
    race == "asian" ~ "Asian"
  ),
  responses10 = responses10 / 100,
  responses20 = responses20 / 100) %>%
  arrange(-responses10) %>%
  select("Tract Racial Majority" = race,
         "2010 Self-Response Rate" = responses10,
         "2020 Self-Response Rate" = responses20,
         "Tracts in 2010" = n10,
         "Tracts in 2020" = n20)

write.csv(chicago_race_output, file = "./output/chicago_chart_output.csv")

# Maps rely on 2020 projected geographies, which are available for download here:
#  https://data.world/uscensusbureau/2020-census-response-rates/workspace
