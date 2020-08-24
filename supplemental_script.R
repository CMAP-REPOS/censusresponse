#### Archive of code not used in the piece -----------------
####  but potentially useful for interested third parties


###  NOTE - this code relies on functions and packages defined and loaded in script.R

## get 2020 and 2010 rates for cities and towns (ALL IN ILLINOIS - State 17)

# Load list of places in the CMAP area
cmap_places <- read.csv("./sources/CMAP_MPA_Places.csv")

# Pull data and filter for places in the CMAP area
response_place <- full_join(
  getCensus(name = "dec/responserate",
            vintage = "2020",
            vars = c("NAME", "GEO_ID", "CRRALL","CRRINT","RESP_DATE"),
            region = "place:*",
            regionin = "state:17"),
  getCensus(name = "dec/responserate",
            vintage = "2010",
            vars = c("NAME", "GEO_ID","FSRR2010"),
            region = "place:*",
            regionin = "state:17"),
  by = c("GEO_ID","state","place", "NAME")) %>% 
  # clean up data, and calculate current difference
  mutate(CRRALL = as.numeric(CRRALL),
         CRRINT = as.numeric(CRRINT),
         dif = CRRALL - FSRR2010,
         GEOID = str_replace(GEO_ID, "1600000US", "")) %>% 
  select(GEOID, GEO_ID, state, place, NAME, CRRALL, CRRINT, RESP_DATE, FSRR2010, dif) %>%
  filter(!(is.na(CRRALL) | is.na(FSRR2010)),GEO_ID %in% cmap_places$GEO_ID)


# get 2020 and 2010 rates for tracts
response_tract <- full_join(
  getCensus(name = "dec/responserate",
            vintage = "2020",
            vars = c("NAME", "GEO_ID", "CRRALL","CRRINT","RESP_DATE"),
            region = "tract:*",
            regionin = "state:17"),
  getCensus(name = "dec/responserate",
            vintage = "2010",
            vars = c("NAME", "GEO_ID","FSRR2010"),
            region = "tract:*",
            regionin = "state:17"),
  by = c("GEO_ID","state","county", "tract", "NAME")) %>% 
  # clean up data, and calculate current difference
  mutate(CRRALL = as.numeric(CRRALL),
         CRRINT = as.numeric(CRRINT),
         dif = CRRALL - FSRR2010,
         GEOID = str_replace(GEO_ID, "1400000US", "")) %>% 
  select(GEOID, GEO_ID, state, county, tract, NAME, CRRALL, CRRINT, RESP_DATE, FSRR2010, dif) %>% 
  # filter for inclusion in CMAP area and remove NA values in response rates
  filter(county %in% cmap_counties,!(is.na(CRRALL) | is.na(FSRR2010)))

## Do quick analysis here of how many tracts/places are missing one response rate or the other?

#missingResponsesTracts <- response_tract %>%
#  filter(is.na(CRRALL) | is.na(FSRR2010)) 

#missingResponsesPlaces <- response_place %>%
#  filter(is.na(CRRALL) | is.na(FSRR2010)) 

## There are only three tracts and eight places with one or both missing. 
##   We cleaned to remove them. None of the eight places are in the CMAP region.



# Pull ACS 5-year data for 2010 and 2018 at the tract level in the CMAP Area
demogs_tract_2010 <- demo_puller(year = 2010, survey = "acs5", stateFIPS = "17",
                                 countyFIPS = cmap_counties, 
                                 ACSvariables = ACSvariables2010)
demogs_tract_2018 <- demo_puller(year = 2018, survey = "acs5", stateFIPS = "17",
                                 countyFIPS = cmap_counties, 
                                 ACSvariables = ACSvariables2018)
demogs_place_2018 <- get_acs(geography = "place", variables = ACSvariables2018, 
                             cache_table = TRUE, year = 2018, state = "17", 
                             survey = "acs5", output = "wide")


# Clean demographic data for tracts and places in the CMAP area
demogs_tract_2010_clean <- demogs_tract_2010_cleaner(demogs_tract_2010)
demogs_tract_2018_clean <- demogs_tract_2018_cleaner(demogs_tract_2018)
demogs_place_2018_clean <- demogs_place_2018_cleaner(demogs_place_2018)



# Relate data to 2020 tracts
crosswalkCMAP <- crosswalk %>%
  # Select Illinois tracts
  filter(STATEFP10 == 17) %>%
  # Select tracts in the 7-county area
  filter(COUNTYFP10 %in% as.numeric(cmap_counties)) %>%
  # Select relevant variables
  select(GEOID10,          # The tract ID for the 2010 tract of the record
         GEOID20,          # The tract ID for the 2020 tract of the record
         HU10PCT_T10,      # Percentage of 2010 housing units associated with the 2010-based tract represented by the record
         HU10PCT_T20,      # Percentage of 2020 housing units associated with the 2010-based tract represented by the record
         HUCURPCT_T10,     # Percentage of current estimated housing units associated with the 2010-based tract represented by the record
         HUCURPCT_T20)     # Percentage of current estimated housing units associated with the 2020-based tract represented by the record


## Interpolate 2010 and 2018 data to 2020 tracts.

# Add 2010 demographic data to the crosswalk table
consolidated_tract2010 <- full_join(crosswalkCMAP, 
                                    demogs_tract_2010_clean, 
                                    by = c("GEOID10" = "GEOID"))

# Add 2018 demographic data to the crosswalk table
consolidated_tract2018 <- full_join(crosswalkCMAP, 
                                    demogs_tract_2018_clean, 
                                    by = c("GEOID10" = "GEOID"))


# Interpolate ACS data to new tracts, using share of housing represented
#   as the weight for a weighted mean

# Interpolate demographic variables: We use the share of housing in the 2020 
#  tracts that was present in each corresponding 2010 tract as a proxy for the 
#  relative weighting of the impact of the source tract on the new tract's 
#  demographic breakdown (for variables that are an average across the 
#  population). For 2010 variables, we use 2010 housing. For 2018 variables, we
#  use "current" housing.
interpolated_tract2010 <- consolidated_tract2010 %>%
  group_by(GEOID20) %>%
  summarize_at(demographic_variables2010, 
               funs(weighted.mean(.,w=HU10PCT_T20,na.rm = TRUE)))

interpolated_tract2018 <- consolidated_tract2018 %>%
  group_by(GEOID20) %>%
  summarize_at(demographic_variables2018, 
               funs(weighted.mean(.,w=HUCURPCT_T20,na.rm = TRUE)))

# Interpolate population and households: We use the share of 2010 housing present  
#  in the portion of the 2010 tract to allocate population into new tract  
#  boundaries. Since this is calculated as a sum, and not an average, it accounts 
#  for cases where two tracts merge or one tract might be split. For 2010 
#  variables, we use 2010 housing. For 2018 variables, we use "current" housing.
interpolated_popul2010 <- consolidated_tract2010 %>%
  group_by(GEOID20) %>%
  summarize(popul = sum(popul * HU10PCT_T10/100,na.rm = TRUE),
            hhold = sum(hhold * HU10PCT_T10/100,na.rm = TRUE),
            hunit = sum(hunit * HU10PCT_T10/100,na.rm = TRUE))

interpolated_popul2018 <- consolidated_tract2018 %>%
  group_by(GEOID20) %>%
  summarize(popul = sum(popul * HUCURPCT_T10/100,na.rm = TRUE),
            hhold = sum(hhold * HUCURPCT_T10/100,na.rm = TRUE),
            hunit = sum(hunit * HUCURPCT_T10/100,na.rm = TRUE))


# Combine the interpolated data, adding suffixes for duplicate demographics
interpolated_tract_combined <- full_join(full_join(interpolated_tract2010,
                                                   interpolated_popul2010,
                                                   by = c("GEOID20")),
                                         full_join(interpolated_tract2018,
                                                   interpolated_popul2018,
                                                   by = c("GEOID20")),
                                         by=c("GEOID20"),
                                         suffix=c("10","18"))



## Check: compare total interpolated 2010 and 2018 population to actuals
abs(1 - sum(interpolated_popul2010$popul,na.rm=TRUE) / 
      sum(demogs_tract_2010_clean$popul,na.rm=TRUE)) < .0001

abs(1 - sum(interpolated_popul2018$popul,na.rm=TRUE) / 
      sum(demogs_tract_2018_clean$popul,na.rm=TRUE)) < .0001

# There are differences of 59 in 2010 and 2 in 2018, which are acceptable (given
#  possible discrepancies due to rounding and missing data). These are less than 
#  0.01%

## Check: compare total interpolated 2010 and 2018 housing units to actuals
abs(1 - sum(interpolated_popul2010$hunit,na.rm=TRUE) / 
      sum(demogs_tract_2010_clean$hunit,na.rm=TRUE)) < .0001

abs(1 - sum(interpolated_popul2018$hunit,na.rm=TRUE) / 
      sum(demogs_tract_2018_clean$hunit,na.rm=TRUE)) < .0001


## Check: compare total interpolated household income to actuals
abs(1 - weighted.mean(interpolated_tract_combined$hhinc10,
                      w=interpolated_tract_combined$popul10,
                      na.rm=TRUE) / 
      weighted.mean(demogs_tract_2010_clean$hhinc,
                    w=demogs_tract_2010_clean$popul,
                    na.rm=TRUE)) < .0001

abs(1 - weighted.mean(interpolated_tract_combined$hhinc18,
                      w=interpolated_tract_combined$popul18,
                      na.rm=TRUE) / 
      weighted.mean(demogs_tract_2018_clean$hhinc,
                    w=demogs_tract_2018_clean$popul,
                    na.rm=TRUE)) < .0001

# There are differences of $1 in 2010 and $2 in 2018, which represent discrepancies 
#  of less than 0.01%. We are comfortable with this level of variation (which could
#  be due to crosswalk discrepancies and/or rounding in the housing share 
#  weights) and proceed using the interpolated numbers.





## Check: individual case of two tracts merging
# Old tracts: 17031490400, 17031490300 (forming 32.2% and 67.8% of the new tract, 
#  respectively, in terms of share of housing units)
# New tract: 17031490200

test_source <- c("17031490400", "17031490300")
test_new <- c("17031490200")

proportions <- crosswalkCMAP %>% filter(GEOID20 %in% test_new)

# Check if population of new merged tract equals population of prior tracts
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(sum = sum(popul10)) %>% 
  pull() -
  
  demogs_tract_2010_clean %>%
  filter(GEOID %in% test_source) %>%
  summarize(sum = sum(popul)) %>%
  pull() < .0001

interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(sum = sum(popul18)) %>% 
  pull() -
  
  demogs_tract_2018_clean %>%
  filter(GEOID %in% test_source) %>%
  summarize(sum = sum(popul)) %>%
  pull() < .0001


# Check if household income is properly interpolated (weighting by share of tract)
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(hhinc = sum(hhinc10)) %>% 
  pull() -
  
  demogs_tract_2010_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(hhinc = sum(hhinc * HU10PCT_T20/100)) %>%
  pull() < .0001

interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(hhinc = sum(hhinc18)) %>% 
  pull() -
  
  demogs_tract_2018_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(hhinc = sum(hhinc * HUCURPCT_T20/100)) %>%
  pull() < .0001


## Check: individual case of one tract splitting into two
# Original tract: 17031806003
# New Tracts: 17031806005, 17031806006

test_source <- c("17031806003")
test_new <- c("17031806005", "17031806006")

# Check if population of prior tract equals total of two new tracts
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(sum = sum(popul10)) %>% 
  pull() -
  
  demogs_tract_2010_clean %>%
  filter(GEOID %in% test_source) %>%
  select(popul) %>%
  pull() < .0001

# Other demographics are inherited from parent tract - check if "white18" matches:
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  mutate(check = white18 == (demogs_tract_2018_clean %>% 
                               filter(GEOID %in% test_source) %>% 
                               select(white) %>%
                               pull())
  ) %>%
  select(check)



## Check: Not a perfect merge/split (i.e., change in tract boundaries)
# Source tracts: 17197980100, 17197884103, 17197880903
# New tract: 17197880903

test_source <- c("17197980100", "17197884103", "17197880903")
test_new <- c("17197880903")

proportions <- crosswalkCMAP %>% filter(GEOID20 %in% test_new)

# Check if population of new tract equals proportionate total of old tracts
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(popul = sum(popul10)) %>% 
  pull() -
  
  demogs_tract_2010_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(popul = sum(popul * HU10PCT_T10/100)) %>%
  pull() < .0001

interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(popul = sum(popul18)) %>% 
  pull() -
  
  demogs_tract_2018_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(popul = sum(popul * HUCURPCT_T10/100)) %>%
  pull() < .0001



# Check if household income of new tract equals proportionate total of old tracts
interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(hhinc = sum(hhinc10)) %>% 
  pull() -
  
  demogs_tract_2010_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(hhinc = sum(hhinc * HU10PCT_T20/100)) %>%
  pull() < .0001

interpolated_tract_combined %>% 
  filter(GEOID20 %in% test_new) %>%
  summarize(hhinc = sum(hhinc18)) %>%
  pull() -
  
  demogs_tract_2018_clean %>%
  filter(GEOID %in% test_source) %>%
  full_join(.,proportions, by=c("GEOID"="GEOID10")) %>%
  summarize(hhinc = sum(hhinc * HUCURPCT_T20/100)) %>%
  pull() < .0001






# Join demographic data to current census response rates (as of August 19)
final_tract <- full_join(interpolated_tract_combined, 
                         response_tract, 
                         by = c("GEOID20" = "GEOID"))

final_place <- left_join(response_place,demogs_place_2018_clean,by = c("GEOID","NAME"))

# Export place data
write.csv(final_place, file = "./output/place_demog_responses.csv")



## Check: Is MSA-analysis consistent with Chicago-specific analysis?

# Are (non-NA) rows equivalent?
truth_check <- final_tract %>% filter(!is.na(GEO_ID)) ==
  final_tract_cmap[,1:33] %>% filter(!is.na(GEO_ID))

# Check that the total number of TRUE (i.e., the matches) is the same as the total
min(colSums(truth_check,na.rm = TRUE)) == nrow(truth_check)



## calculate a regional average (note this will differ because it is based on
#   county-wide (and more recent) household totals from the ACS 1 year estimates,
#   and rolls up a regional average using county-only rates vs. tract-level rates))


data_as_of_819 <- read.csv("./sources/08-19-2020RRData.csv")

counties_as_of_819 <- data_as_of_819 %>%
  rename(GEO_ID = ï..GEO_ID) %>%
  filter(grepl("0500000US",GEO_ID)) %>%
  separate(GEO_ID,into = c("prefix","state","county"),sep=c(9,11)) %>%
  filter(state == "17",
         county %in% cmap_counties) %>%
  select(state,
         county,
         CRRALL)

response_region <- full_join(
  getCensus(name = "dec/responserate",
            vintage = "2020",
            vars = c("NAME","GEO_ID","CRRALL","CRRINT","RESP_DATE"),
            region = "county:*",
            regionin = "state:17"),
  getCensus(name = "dec/responserate",
            vintage = "2010",
            vars = c("NAME","GEO_ID","FSRR2010"),
            region = "county:*",
            regionin = "state:17"),
  by = c("GEO_ID","state","county","NAME")) %>%
  filter(county %in% cmap_counties)

demogs_region_2018 <- get_acs(geography = "county", variables = "B25001_001", 
                              cache_table = TRUE, year = 2018, state = "17", 
                              survey = "acs1", output = "wide") %>%
  filter(NAME %in% response_region$NAME) %>%
  select(GEOID,
         NAME,
         hunit18 = B25001_001E)

demogs_region_2010 <- get_acs(geography = "county", variables = "B25001_001", 
                              cache_table = TRUE, year = 2010, state = "17", 
                              survey = "acs1", output = "wide") %>%
  filter(NAME %in% response_region$NAME) %>%
  select(GEOID,
         NAME,
         hunit10 = B25001_001E)

hunit_region <- full_join(demogs_region_2010,demogs_region_2018,by=c("GEOID","NAME")) %>%
  separate(GEOID, into = c("state", "county"),
           sep = c(2), remove = FALSE) %>%
  select(state,county,NAME,hunit10,hunit18)

final_region <- full_join(hunit_region,response_region,by=c("state","county","NAME")) %>%
  mutate(CRRALL = as.numeric(CRRALL),
         FSRR2010 = as.numeric(FSRR2010)) %>%
  cbind(.,demogs_tract_2018_clean %>% group_by(county) %>% summarize(sum = sum(hunit)) %>% select(hunit18alt = sum)) %>%
  cbind(.,demogs_tract_2010_clean %>% group_by(county) %>% summarize(sum = sum(hunit)) %>% select(hunit10alt = sum)) %>%
  cbind(.,counties_as_of_819 %>% select(CRRALL0819 = CRRALL))


## calculate regional average in 2020 (for reference and comparison)

# As of latest data
final_region %>%
  summarize(response = weighted.mean(x=CRRALL,w=hunit18))

# As of latest data, using sum of tract-level (5-year) housing units
final_region %>%
  summarize(response = weighted.mean(x=CRRALL, w = hunit18alt))

# As of 8/19
final_region %>%
  summarize(response = weighted.mean(x = CRRALL0819, w=hunit18))

## calculate regional average in 2010

# Using county-level housing units
final_region %>%
  summarize(response = weighted.mean(x=FSRR2010,w=hunit10))

# Using sum of tract-level housing units
final_region %>%
  summarize(response = weighted.mean(x=FSRR2010, w = hunit10alt))

# Show source of discrepancies (it appears that Cook has the largest discrepancy, and thus
#  contributes the most both absolutely and relatively to the overall 0.9 pp shift)
final_tract_cmap %>% group_by(county) %>% summarize(sum = weighted.mean(x=FSRR2010,w=hunit10)) %>% 
  left_join(.,final_region %>% select(county,FSRR2010,NAME),by="county")
