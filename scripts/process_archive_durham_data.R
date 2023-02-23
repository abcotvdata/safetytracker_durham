library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(pdftools)
library(lubridate)

### PROCESS THE FINAL WEEKLY FILE FROM DURHAM PD in 2021 #####
# This file contains the total for 2019

# Load the file
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
past_crime_durham <- past_crime_durham %>% unnest_wider(rawtext2, names_sep = "_")

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

# clean to convert to numeric; remove any NAs
past_crime_durham$total19 <- gsub(",","",past_crime_durham$total19)
past_crime_durham$total19 <- as.numeric(past_crime_durham$total19)
past_crime_durham[is.na(past_crime_durham)] <- 0

# export this file as RDS store for use in tracker building
saveRDS(past_crime_durham,"scripts/rds/past_crime_durham.RDS")
