library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(pdftools)
library(lubridate)

### PAGE 1 ###

# Load the file we want for 2021 (December / Year End)
pdftext <- pdf_text("data/source/recent/November 5 2022 Weekly Period to Date Table.pdf") %>% strsplit(split = "\n")

# Grab individual text values for Page 1
rawtext1 <- pdftext[[1]][64] %>% trimws()
rawtext2 <- pdftext[[1]][65] %>% trimws()
rawtext3 <- pdftext[[1]][66] %>% trimws()
rawtext4 <- pdftext[[1]][67] %>% trimws()
rawtext5 <- pdftext[[1]][68] %>% trimws()
rawtext6 <- pdftext[[1]][74] %>% trimws()
rawtext7 <- pdftext[[1]][75] %>% trimws()
rawtext8 <- pdftext[[1]][79] %>% trimws()

rawtext9 <- pdftext[[1]][82] %>% trimws()
rawtext10 <- pdftext[[1]][83] %>% trimws()
rawtext11 <- pdftext[[1]][84] %>% trimws()
rawtext12 <- pdftext[[1]][85] %>% trimws()
rawtext13 <- pdftext[[1]][86] %>% trimws()
rawtext14 <- pdftext[[1]][92] %>% trimws()
rawtext15 <- pdftext[[1]][93] %>% trimws()
rawtext16 <- pdftext[[1]][97] %>% trimws()

rawtext17 <- pdftext[[1]][100] %>% trimws()
rawtext18 <- pdftext[[1]][101] %>% trimws()
rawtext19 <- pdftext[[1]][102] %>% trimws()
rawtext20 <- pdftext[[1]][103] %>% trimws()
rawtext21 <- pdftext[[1]][104] %>% trimws()
rawtext22 <- pdftext[[1]][110] %>% trimws()
rawtext23 <- pdftext[[1]][111] %>% trimws()
rawtext24 <- pdftext[[1]][115] %>% trimws()

rawtext25 <- pdftext[[2]][64] %>% trimws()
rawtext26 <- pdftext[[2]][65] %>% trimws()
rawtext27 <- pdftext[[2]][66] %>% trimws()
rawtext28 <- pdftext[[2]][67] %>% trimws()
rawtext29 <- pdftext[[2]][68] %>% trimws()
rawtext30 <- pdftext[[2]][74] %>% trimws()
rawtext31 <- pdftext[[2]][75] %>% trimws()
rawtext32 <- pdftext[[2]][79] %>% trimws()

rawtext33 <- pdftext[[2]][82] %>% trimws()
rawtext34 <- pdftext[[2]][83] %>% trimws()
rawtext35 <- pdftext[[2]][84] %>% trimws()
rawtext36 <- pdftext[[2]][85] %>% trimws()
rawtext37 <- pdftext[[2]][86] %>% trimws()
rawtext38 <- pdftext[[2]][92] %>% trimws()
rawtext39 <- pdftext[[2]][93] %>% trimws()
rawtext40 <- pdftext[[2]][97] %>% trimws()

rawtext41 <- pdftext[[2]][100] %>% trimws()
rawtext42 <- pdftext[[2]][101] %>% trimws()
rawtext43 <- pdftext[[2]][102] %>% trimws()
rawtext44 <- pdftext[[2]][103] %>% trimws()
rawtext45 <- pdftext[[2]][104] %>% trimws()
rawtext46 <- pdftext[[2]][110] %>% trimws()
rawtext47 <- pdftext[[2]][111] %>% trimws()
rawtext48 <- pdftext[[2]][115] %>% trimws()

rawtext49 <- pdftext[[1]][2] %>% trimws()
rawtext50 <- pdftext[[2]][2] %>% trimws()

# Bind those into a one-column table
recent_crime_durham <- rbind(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
rm(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
recent_crime_durham <- as.data.frame(recent_crime_durham)

# name the column temporarily
names(recent_crime_durham) <- c("rawtext")
# remove the long white space on each end of each line
recent_crime_durham$rawtext2 <- strsplit(recent_crime_durham$rawtext, "\\s+\\s+")
# flatten the list this creates in processed column
recent_crime_durham <- recent_crime_durham %>% unnest_wider(rawtext2)

# get as of date out of this table now
asofdate <- recent_crime_durham[49,1]
asofdate_doublecheck <- recent_crime_durham[50,1]

# name the columns temporarily
names(recent_crime_durham) = c("rawtext","category","this_month_so_far",
                              "last_month","total20","total21",
                              "change20to21","average17to21","ytd20",
                              "ytd21","ytd22","change21to22",
                              "percent_of_total")

# create a district column; reformat and fill
recent_crime_durham$district <- recent_crime_durham$category
recent_crime_durham$district <- case_when(recent_crime_durham$district == "District 1" ~ "D1",
                                          recent_crime_durham$district == "District 2" ~ "D2",
                                          recent_crime_durham$district == "District 3" ~ "D3",
                                          recent_crime_durham$district == "District 4" ~ "D4",
                                          recent_crime_durham$district == "District 5" ~ "D5",
                                          recent_crime_durham$district == "Totals" ~ "Citywide",
                                          TRUE ~ "")
recent_crime_durham[recent_crime_durham == ""] <- NA
recent_crime_durham <- recent_crime_durham %>% fill(district)

# Drop extra rows of unneeded headers and text
recent_crime_durham <- recent_crime_durham %>% filter(category %in% 
                                                      c("1. Criminal Homicide",
                                                        "2. Forcible Rape",
                                                        "3. Robbery",
                                                        "4. Aggravated Assault*",
                                                        "5. Burglary-Breaking & Entering",
                                                        "6. Larceny - Theft (Except Motor Vehicles)",
                                                        "7. Motor Vehicle Theft"))

recent_crime_durham$type <- case_when(recent_crime_durham$category == "1. Criminal Homicide" ~ "Violent",
                                      recent_crime_durham$category == "2. Forcible Rape" ~ "Violent",
                                      recent_crime_durham$category == "3. Robbery" ~ "Violent",
                                      recent_crime_durham$category == "4. Aggravated Assault*" ~ "Violent",
                                      recent_crime_durham$category == "5. Burglary-Breaking & Entering" ~ "Property",
                                      recent_crime_durham$category == "6. Larceny - Theft (Except Motor Vehicles)" ~ "Property",
                                      recent_crime_durham$category == "7. Motor Vehicle Theft" ~ "Property",
                                    TRUE ~ "Other")

recent_crime_durham$category <- case_when(recent_crime_durham$category == "1. Criminal Homicide" ~ "Murder",
                                          recent_crime_durham$category == "2. Forcible Rape" ~ "Sexual Assault",
                                          recent_crime_durham$category == "3. Robbery" ~ "Robbery",
                                          recent_crime_durham$category == "4. Aggravated Assault*" ~ "Aggravated Assault",
                                          recent_crime_durham$category == "5. Burglary-Breaking & Entering" ~ "Burglary",
                                          recent_crime_durham$category == "6. Larceny - Theft (Except Motor Vehicles)" ~ "Theft",
                                          recent_crime_durham$category == "7. Motor Vehicle Theft" ~ "Vehicle Theft",
                                          TRUE ~ "Other")

# Drop extra columns of unneeded data
recent_crime_durham <- recent_crime_durham %>% select(2,5,6,10,11,14,15)

recent_crime_durham$ytd21 <- gsub(",","",recent_crime_durham$ytd21)
recent_crime_durham$ytd22 <- gsub(",","",recent_crime_durham$ytd22)
recent_crime_durham$total20 <- gsub(",","",recent_crime_durham$total20)
recent_crime_durham$total21 <- gsub(",","",recent_crime_durham$total21)

recent_crime_durham$ytd21 <- as.numeric(recent_crime_durham$ytd21)
recent_crime_durham$ytd22 <- as.numeric(recent_crime_durham$ytd22)
recent_crime_durham$total20 <- as.numeric(recent_crime_durham$total20)
recent_crime_durham$total21 <- as.numeric(recent_crime_durham$total21)

recent_crime_durham[is.na(recent_crime_durham)] <- 0

### repeat for past crime
# This will add annual figure for 2019

# Load the file we want for 2021 (December / Year End)
pdftext <- pdf_text("data/source/annual/Period to DateDec25.pdf") %>% strsplit(split = "\n")

# Grab individual text values for Page 1
rawtext1 <- pdftext[[1]][56] %>% trimws()
rawtext2 <- pdftext[[1]][57] %>% trimws()
rawtext3 <- pdftext[[1]][58] %>% trimws()
rawtext4 <- pdftext[[1]][59] %>% trimws()
rawtext5 <- pdftext[[1]][60] %>% trimws()
rawtext6 <- pdftext[[1]][66] %>% trimws()
rawtext7 <- pdftext[[1]][67] %>% trimws()
rawtext8 <- pdftext[[1]][71] %>% trimws()

rawtext9 <- pdftext[[1]][74] %>% trimws()
rawtext10 <- pdftext[[1]][75] %>% trimws()
rawtext11 <- pdftext[[1]][76] %>% trimws()
rawtext12 <- pdftext[[1]][77] %>% trimws()
rawtext13 <- pdftext[[1]][78] %>% trimws()
rawtext14 <- pdftext[[1]][84] %>% trimws()
rawtext15 <- pdftext[[1]][85] %>% trimws()
rawtext16 <- pdftext[[1]][89] %>% trimws()

rawtext17 <- pdftext[[1]][92] %>% trimws()
rawtext18 <- pdftext[[1]][93] %>% trimws()
rawtext19 <- pdftext[[1]][94] %>% trimws()
rawtext20 <- pdftext[[1]][95] %>% trimws()
rawtext21 <- pdftext[[1]][96] %>% trimws()
rawtext22 <- pdftext[[1]][102] %>% trimws()
rawtext23 <- pdftext[[1]][103] %>% trimws()
rawtext24 <- pdftext[[1]][107] %>% trimws()

rawtext25 <- pdftext[[2]][56] %>% trimws()
rawtext26 <- pdftext[[2]][57] %>% trimws()
rawtext27 <- pdftext[[2]][58] %>% trimws()
rawtext28 <- pdftext[[2]][59] %>% trimws()
rawtext29 <- pdftext[[2]][60] %>% trimws()
rawtext30 <- pdftext[[2]][66] %>% trimws()
rawtext31 <- pdftext[[2]][67] %>% trimws()
rawtext32 <- pdftext[[2]][71] %>% trimws()

rawtext33 <- pdftext[[2]][74] %>% trimws()
rawtext34 <- pdftext[[2]][75] %>% trimws()
rawtext35 <- pdftext[[2]][76] %>% trimws()
rawtext36 <- pdftext[[2]][77] %>% trimws()
rawtext37 <- pdftext[[2]][78] %>% trimws()
rawtext38 <- pdftext[[2]][84] %>% trimws()
rawtext39 <- pdftext[[2]][85] %>% trimws()
rawtext40 <- pdftext[[2]][89] %>% trimws()

rawtext41 <- pdftext[[2]][92] %>% trimws()
rawtext42 <- pdftext[[2]][93] %>% trimws()
rawtext43 <- pdftext[[2]][94] %>% trimws()
rawtext44 <- pdftext[[2]][95] %>% trimws()
rawtext45 <- pdftext[[2]][96] %>% trimws()
rawtext46 <- pdftext[[2]][102] %>% trimws()
rawtext47 <- pdftext[[2]][103] %>% trimws()
rawtext48 <- pdftext[[2]][107] %>% trimws()

rawtext49 <- pdftext[[1]][2] %>% trimws()
rawtext50 <- pdftext[[2]][2] %>% trimws()

# Bind those into a one-column table
past_crime_durham <- rbind(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
# rm(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
past_crime_durham <- as.data.frame(past_crime_durham)

# name the column temporarily
names(past_crime_durham) <- c("rawtext")
# remove the long white space on each end of each line
past_crime_durham$rawtext2 <- strsplit(past_crime_durham$rawtext, "\\s+\\s+")
# flatten the list this creates in processed column
past_crime_durham <- past_crime_durham %>% unnest_wider(rawtext2)

# get as of date out of this table now
past_year_filedate <- past_crime_durham[49,1]
past_year_filedate_doublecheck <- past_crime_durham[50,1]

# name the columns temporarily
names(past_crime_durham) = c("rawtext","category","this_month_so_far",
                               "last_month","total19","total20",
                               "change19to20","average16to20","ytd19",
                               "ytd20","ytd21","change20to21",
                               "percent_of_total")

# create a district column; reformat and fill
past_crime_durham$district <- past_crime_durham$category
past_crime_durham$district <- case_when(past_crime_durham$district == "District 1" ~ "D1",
                                          past_crime_durham$district == "District 2" ~ "D2",
                                          past_crime_durham$district == "District 3" ~ "D3",
                                          past_crime_durham$district == "District 4" ~ "D4",
                                          past_crime_durham$district == "District 5" ~ "D5",
                                          past_crime_durham$district == "Totals" ~ "Citywide",
                                          TRUE ~ "")
past_crime_durham[past_crime_durham == ""] <- NA
past_crime_durham <- past_crime_durham %>% fill(district)

# Drop extra rows of unneeded headers and text
past_crime_durham <- past_crime_durham %>% filter(category %in% 
                                                        c("1. Criminal Homicide",
                                                          "2. Forcible Rape",
                                                          "3. Robbery",
                                                          "4. Aggravated Assault*",
                                                          "5. Burglary-Breaking & Entering",
                                                          "6. Larceny - Theft (Except Motor Vehicles)",
                                                          "7. Motor Vehicle Theft"))

past_crime_durham$type <- case_when(past_crime_durham$category == "1. Criminal Homicide" ~ "Violent",
                                        past_crime_durham$category == "2. Forcible Rape" ~ "Violent",
                                        past_crime_durham$category == "3. Robbery" ~ "Violent",
                                        past_crime_durham$category == "4. Aggravated Assault*" ~ "Violent",
                                        past_crime_durham$category == "5. Burglary-Breaking & Entering" ~ "Property",
                                        past_crime_durham$category == "6. Larceny - Theft (Except Motor Vehicles)" ~ "Property",
                                        past_crime_durham$category == "7. Motor Vehicle Theft" ~ "Property",
                                        TRUE ~ "Other")

past_crime_durham$category <- case_when(past_crime_durham$category == "1. Criminal Homicide" ~ "Murder",
                                          past_crime_durham$category == "2. Forcible Rape" ~ "Sexual Assault",
                                          past_crime_durham$category == "3. Robbery" ~ "Robbery",
                                          past_crime_durham$category == "4. Aggravated Assault*" ~ "Aggravated Assault",
                                          past_crime_durham$category == "5. Burglary-Breaking & Entering" ~ "Burglary",
                                          past_crime_durham$category == "6. Larceny - Theft (Except Motor Vehicles)" ~ "Theft",
                                          past_crime_durham$category == "7. Motor Vehicle Theft" ~ "Vehicle Theft",
                                          TRUE ~ "Other")

# Drop extra columns of unneeded data
past_crime_durham <- past_crime_durham %>% select(2,5,14,15)

past_crime_durham$total19 <- gsub(",","",past_crime_durham$total19)

past_crime_durham$total19 <- as.numeric(past_crime_durham$total19)

past_crime_durham[is.na(past_crime_durham)] <- 0


## combine two processed files


durham_crime <- left_join(past_crime_durham,recent_crime_durham,by=c("district"="district","category"="category","type"="type"))
durham_crime$last12mos <- (durham_crime$total21-durham_crime$ytd21)+durham_crime$ytd22

durham_crime <- durham_crime %>% 
  select(3,1,2,5,6,8,9,4)


### CITYWIDE CRIME 
### TOTALS AND OUTPUT

# Set variable of city population
# likely needs added to the tracker itself
durham_population <- 285527

# OPEN WORK
# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- str_replace(asofdate,"Actual Offenses - Period Ending ","")
asofdate <- mdy(asofdate)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# Merge the precincts file with geography and populations
district_crime <- full_join(districts_geo, durham_crime, by="district") %>% filter(district!="DSO")
# add zeros where there were no crimes tallied that year
district_crime$population <- ifelse(district_crime$district=="Citywide", durham_population,district_crime$population)

# add 3-year totals and annualized averages
district_crime$total_prior3years <- district_crime$total19+
  district_crime$total20+
  district_crime$total21
district_crime$avg_prior3years <- round(((district_crime$total19+
                                            district_crime$total20+
                                            district_crime$total21)/3),1)
# now add the increases or change percentages
district_crime$inc_19to21 <- round(district_crime$total21/district_crime$total19*100-100,1)
district_crime$inc_19tolast12 <- round(district_crime$last12mos/district_crime$total19*100-100,1)
district_crime$inc_21tolast12 <- round(district_crime$last12mos/district_crime$total21*100-100,1)
district_crime$inc_prior3yearavgtolast12 <- round((district_crime$last12mos/district_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
district_crime$rate19 <- round((district_crime$total19/district_crime$population)*100000,1)
district_crime$rate20 <- round((district_crime$total20/district_crime$population)*100000,1)
district_crime$rate21 <- round((district_crime$total21/district_crime$population)*100000,1)
district_crime$rate_last12 <- round((district_crime$last12mos/district_crime$population)*100000,1)
district_crime$rate_prior3years <- 
  round((district_crime$avg_prior3years/district_crime$population)*100000,1)

# Now reduce the precinct down to just the columns we likely need for the tracker pages
# district_crime <- district_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Divide into citywide_crime and district_crime files
citywide_crime <- district_crime %>% filter(district=="Citywide")
district_crime <- district_crime %>% filter(district!="Citywide")

# create a quick long-term annual table
district_yearly <- district_crime %>% select(1,2:6,8) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/district_yearly.csv")

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(1,2:6,8) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/citywide_yearly.csv")

# add additional years from state archive of reported ucr crimes back to 2000
# yearly_archive <- read_csv("data/source/annual/sf_annual_state.csv")
# citywide_yearly <- right_join(citywide_yearly,yearly_archive %>% select(1:18,23),by="category") %>% select(1,7:24,2:6)

# Now make individual crime files for trackers
# filter precinct versions - using beat for code consistency
murders_district <- district_crime %>% filter(category=="Murder")
sexassaults_district <- district_crime %>% filter(category=="Sexual Assault")
robberies_district <- district_crime %>% filter(category=="Robbery")
assaults_district <- district_crime %>% filter(category=="Aggravated Assault")
burglaries_district <- district_crime %>% filter(category=="Burglary")
thefts_district <- district_crime %>% filter(category=="Theft")
autothefts_district <- district_crime %>% filter(category=="Vehicle Theft")
# filter citywide versions
murders_city <- citywide_crime %>% filter(category=="Murder")
sexassaults_city <- citywide_crime %>% filter(category=="Sexual Assault")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Theft")
autothefts_city <- citywide_crime %>% filter(category=="Vehicle Theft")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
write_csv(district_crime,"data/output/weekly/district_crime.csv")
write_csv(citywide_crime,"data/output/weekly/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(district_crime,paste0("data/output/archive/district_crime_week",asofdate,".csv"))
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week",asofdate,".csv"))
# Now save the files needed for trackers into RDS store in scripts for GH Actions
# precinct versions
saveRDS(district_crime,"scripts/rds/district_crime.rds")
saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
# city versions
saveRDS(citywide_crime,"scripts/rds/citywide_crime.rds")
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")

### Some tables for charts for our pages
citywide_yearly %>% filter(category=="Murder") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_yearly %>% filter(category=="Rape") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_yearly %>% filter(category=="Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_yearly %>% filter(category=="Theft") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_yearly %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_yearly %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_yearly %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")

# deaths cause data update for North Carolina specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="NC")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")
