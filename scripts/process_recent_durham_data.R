library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(pdftools)
library(lubridate)

### PROCESS THE NEWEST WEEKLY FILE FROM DURHAM PD #####
# source is https://www.durhamnc.gov/Archive.aspx?AMID=150
# Load the file we want for 2022 (This Week)
pdftext <- pdf_text("data/source/recent/December 31 2022 Weekly Period to Date Table.pdf") %>% strsplit(split = "\n")

# Grab individual text values for each of 5 districts plus citywide
# Dist 1
rawtext1 <- pdftext[[1]][62] %>% trimws()
rawtext2 <- pdftext[[1]][63] %>% trimws()
rawtext3 <- pdftext[[1]][64] %>% trimws()
rawtext4 <- pdftext[[1]][65] %>% trimws()
rawtext5 <- pdftext[[1]][66] %>% trimws()
rawtext6 <- pdftext[[1]][72] %>% trimws()
rawtext7 <- pdftext[[1]][73] %>% trimws()
rawtext8 <- pdftext[[1]][77] %>% trimws()
# Dist 2
rawtext9 <- pdftext[[1]][80] %>% trimws()
rawtext10 <- pdftext[[1]][81] %>% trimws()
rawtext11 <- pdftext[[1]][82] %>% trimws()
rawtext12 <- pdftext[[1]][83] %>% trimws()
rawtext13 <- pdftext[[1]][84] %>% trimws()
rawtext14 <- pdftext[[1]][90] %>% trimws()
rawtext15 <- pdftext[[1]][91] %>% trimws()
rawtext16 <- pdftext[[1]][95] %>% trimws()
# Dist 3
rawtext17 <- pdftext[[1]][98] %>% trimws()
rawtext18 <- pdftext[[1]][99] %>% trimws()
rawtext19 <- pdftext[[1]][100] %>% trimws()
rawtext20 <- pdftext[[1]][101] %>% trimws()
rawtext21 <- pdftext[[1]][102] %>% trimws()
rawtext22 <- pdftext[[1]][108] %>% trimws()
rawtext23 <- pdftext[[1]][109] %>% trimws()
rawtext24 <- pdftext[[1]][113] %>% trimws()
# Dist 4
rawtext25 <- pdftext[[2]][62] %>% trimws()
rawtext26 <- pdftext[[2]][63] %>% trimws()
rawtext27 <- pdftext[[2]][64] %>% trimws()
rawtext28 <- pdftext[[2]][65] %>% trimws()
rawtext29 <- pdftext[[2]][66] %>% trimws()
rawtext30 <- pdftext[[2]][72] %>% trimws()
rawtext31 <- pdftext[[2]][73] %>% trimws()
rawtext32 <- pdftext[[2]][77] %>% trimws()
# Dist 5
rawtext33 <- pdftext[[2]][80] %>% trimws()
rawtext34 <- pdftext[[2]][81] %>% trimws()
rawtext35 <- pdftext[[2]][82] %>% trimws()
rawtext36 <- pdftext[[2]][83] %>% trimws()
rawtext37 <- pdftext[[2]][84] %>% trimws()
rawtext38 <- pdftext[[2]][90] %>% trimws()
rawtext39 <- pdftext[[2]][91] %>% trimws()
rawtext40 <- pdftext[[2]][95] %>% trimws()
# Citywide
rawtext41 <- pdftext[[2]][98] %>% trimws()
rawtext42 <- pdftext[[2]][99] %>% trimws()
rawtext43 <- pdftext[[2]][100] %>% trimws()
rawtext44 <- pdftext[[2]][101] %>% trimws()
rawtext45 <- pdftext[[2]][102] %>% trimws()
rawtext46 <- pdftext[[2]][108] %>% trimws()
rawtext47 <- pdftext[[2]][109] %>% trimws()
rawtext48 <- pdftext[[2]][113] %>% trimws()
# Dates and backup check
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

# prep for conversion to numeric by removing commas
recent_crime_durham$ytd21 <- gsub(",","",recent_crime_durham$ytd21)
recent_crime_durham$ytd22 <- gsub(",","",recent_crime_durham$ytd22)
recent_crime_durham$total20 <- gsub(",","",recent_crime_durham$total20)
recent_crime_durham$total21 <- gsub(",","",recent_crime_durham$total21)

# convert the number cols to numeric
recent_crime_durham$ytd21 <- as.numeric(recent_crime_durham$ytd21)
recent_crime_durham$ytd22 <- as.numeric(recent_crime_durham$ytd22)
recent_crime_durham$total20 <- as.numeric(recent_crime_durham$total20)
recent_crime_durham$total21 <- as.numeric(recent_crime_durham$total21)

# remove NAs and replace with zeros if any exist
recent_crime_durham[is.na(recent_crime_durham)] <- 0

# export this file as RDS store for use in tracker building
saveRDS(recent_crime_durham,"scripts/rds/recent_crime_durham.RDS")

# OPEN WORK
# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- str_replace(asofdate,"Actual Offenses - Period Ending ","")
asofdate <- mdy(asofdate)
saveRDS(asofdate,"scripts/rds/asofdate.rds")