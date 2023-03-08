library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(pdftools)
library(lubridate)

### PROCESS THE NEWEST WEEKLY FILE FROM DURHAM PD #####
# source is https://www.durhamnc.gov/Archive.aspx?AMID=150
# Load the file we want for 2022 (This Week)
pdftext <- pdf_text("data/source/recent/period_to_date.pdf") %>% strsplit(split = "\n")

# Grab individual text values for each of 5 districts plus citywide
rawtext1 <- pdftext[[1]][50] %>% trimws()
rawtext2 <- pdftext[[1]][51] %>% trimws()
rawtext3 <- pdftext[[1]][52] %>% trimws()
rawtext4 <- pdftext[[1]][53] %>% trimws()
rawtext5 <- pdftext[[1]][54] %>% trimws()
rawtext6 <- pdftext[[1]][55] %>% trimws()
rawtext7 <- pdftext[[1]][56] %>% trimws()
rawtext8 <- pdftext[[1]][57] %>% trimws()
rawtext9 <- pdftext[[1]][58] %>% trimws()
rawtext10 <- pdftext[[1]][59] %>% trimws()
rawtext11 <- pdftext[[1]][60] %>% trimws()
rawtext12 <- pdftext[[1]][61] %>% trimws()
rawtext13 <- pdftext[[1]][62] %>% trimws()
rawtext14 <- pdftext[[1]][63] %>% trimws()
rawtext15 <- pdftext[[1]][64] %>% trimws()
rawtext16 <- pdftext[[1]][65] %>% trimws()
rawtext17 <- pdftext[[1]][66] %>% trimws()
rawtext18 <- pdftext[[1]][67] %>% trimws()
rawtext19 <- pdftext[[1]][68] %>% trimws()
rawtext20 <- pdftext[[1]][69] %>% trimws()
rawtext21 <- pdftext[[1]][70] %>% trimws()
rawtext22 <- pdftext[[1]][71] %>% trimws()
rawtext23 <- pdftext[[1]][72] %>% trimws()
rawtext24 <- pdftext[[1]][73] %>% trimws()
rawtext25 <- pdftext[[1]][74] %>% trimws()
rawtext26 <- pdftext[[1]][75] %>% trimws()
rawtext27 <- pdftext[[1]][76] %>% trimws()
rawtext28 <- pdftext[[1]][77] %>% trimws()
rawtext29 <- pdftext[[1]][78] %>% trimws()
rawtext30 <- pdftext[[1]][79] %>% trimws()
rawtext31 <- pdftext[[1]][80] %>% trimws()
rawtext32 <- pdftext[[1]][81] %>% trimws()
rawtext33 <- pdftext[[1]][82] %>% trimws()
rawtext34 <- pdftext[[1]][83] %>% trimws()
rawtext35 <- pdftext[[1]][84] %>% trimws()
rawtext36 <- pdftext[[1]][85] %>% trimws()
rawtext37 <- pdftext[[1]][86] %>% trimws()
rawtext38 <- pdftext[[1]][87] %>% trimws()
rawtext39 <- pdftext[[1]][88] %>% trimws()
rawtext40 <- pdftext[[1]][89] %>% trimws()
rawtext41 <- pdftext[[1]][90] %>% trimws()
rawtext42 <- pdftext[[1]][91] %>% trimws()
rawtext43 <- pdftext[[1]][92] %>% trimws()
rawtext44 <- pdftext[[1]][93] %>% trimws()
rawtext45 <- pdftext[[1]][94] %>% trimws()
rawtext46 <- pdftext[[1]][95] %>% trimws()
rawtext47 <- pdftext[[1]][96] %>% trimws()
rawtext48 <- pdftext[[1]][97] %>% trimws()
rawtext49 <- pdftext[[1]][98] %>% trimws()
rawtext50 <- pdftext[[1]][99] %>% trimws()
rawtext51 <- pdftext[[1]][100] %>% trimws()
rawtext52 <- pdftext[[1]][101] %>% trimws()
rawtext53 <- pdftext[[1]][102] %>% trimws()
rawtext54 <- pdftext[[1]][103] %>% trimws()
rawtext55 <- pdftext[[1]][104] %>% trimws()
rawtext56 <- pdftext[[1]][105] %>% trimws()
rawtext57 <- pdftext[[1]][106] %>% trimws()
rawtext58 <- pdftext[[1]][107] %>% trimws()
rawtext59 <- pdftext[[1]][108] %>% trimws()
rawtext60 <- pdftext[[1]][109] %>% trimws()
rawtext61 <- pdftext[[1]][110] %>% trimws()
rawtext62 <- pdftext[[1]][111] %>% trimws()
rawtext63 <- pdftext[[1]][112] %>% trimws()
rawtext64 <- pdftext[[1]][113] %>% trimws()
rawtext65 <- pdftext[[1]][114] %>% trimws()
rawtext66 <- pdftext[[1]][115] %>% trimws()
rawtext67 <- pdftext[[1]][116] %>% trimws()
rawtext68 <- pdftext[[1]][117] %>% trimws()
rawtext69 <- pdftext[[1]][118] %>% trimws()
rawtext70 <- pdftext[[1]][119] %>% trimws()
rawtext71 <- pdftext[[1]][120] %>% trimws()
rawtext72 <- pdftext[[2]][50] %>% trimws()
rawtext73 <- pdftext[[2]][51] %>% trimws()
rawtext74 <- pdftext[[2]][52] %>% trimws()
rawtext75 <- pdftext[[2]][53] %>% trimws()
rawtext76 <- pdftext[[2]][54] %>% trimws()
rawtext77 <- pdftext[[2]][55] %>% trimws()
rawtext78 <- pdftext[[2]][56] %>% trimws()
rawtext79 <- pdftext[[2]][57] %>% trimws()
rawtext80 <- pdftext[[2]][58] %>% trimws()
rawtext81 <- pdftext[[2]][59] %>% trimws()
rawtext82 <- pdftext[[2]][60] %>% trimws()
rawtext83 <- pdftext[[2]][61] %>% trimws()
rawtext84 <- pdftext[[2]][62] %>% trimws()
rawtext85 <- pdftext[[2]][63] %>% trimws()
rawtext86 <- pdftext[[2]][64] %>% trimws()
rawtext87 <- pdftext[[2]][65] %>% trimws()
rawtext88 <- pdftext[[2]][66] %>% trimws()
rawtext89 <- pdftext[[2]][67] %>% trimws()
rawtext90 <- pdftext[[2]][68] %>% trimws()
rawtext91 <- pdftext[[2]][69] %>% trimws()
rawtext92 <- pdftext[[2]][70] %>% trimws()
rawtext93 <- pdftext[[2]][71] %>% trimws()
rawtext94 <- pdftext[[2]][72] %>% trimws()
rawtext95 <- pdftext[[2]][73] %>% trimws()
rawtext96 <- pdftext[[2]][74] %>% trimws()
rawtext97 <- pdftext[[2]][75] %>% trimws()
rawtext98 <- pdftext[[2]][76] %>% trimws()
rawtext99 <- pdftext[[2]][77] %>% trimws()
rawtext100 <- pdftext[[2]][78] %>% trimws()
rawtext101 <- pdftext[[2]][79] %>% trimws()
rawtext102 <- pdftext[[2]][80] %>% trimws()
rawtext103 <- pdftext[[2]][81] %>% trimws()
rawtext104 <- pdftext[[2]][82] %>% trimws()
rawtext105 <- pdftext[[2]][83] %>% trimws()
rawtext106 <- pdftext[[2]][84] %>% trimws()
rawtext107 <- pdftext[[2]][85] %>% trimws()
rawtext108 <- pdftext[[2]][86] %>% trimws()
rawtext109 <- pdftext[[2]][87] %>% trimws()
rawtext110 <- pdftext[[2]][88] %>% trimws()
rawtext111 <- pdftext[[2]][89] %>% trimws()
rawtext112 <- pdftext[[2]][90] %>% trimws()
rawtext113 <- pdftext[[2]][91] %>% trimws()
rawtext114 <- pdftext[[2]][92] %>% trimws()
rawtext115 <- pdftext[[2]][93] %>% trimws()
rawtext116 <- pdftext[[2]][94] %>% trimws()
rawtext117 <- pdftext[[2]][95] %>% trimws()
rawtext118 <- pdftext[[2]][96] %>% trimws()
rawtext119 <- pdftext[[2]][97] %>% trimws()
rawtext120 <- pdftext[[2]][98] %>% trimws()
rawtext121 <- pdftext[[2]][99] %>% trimws()
rawtext122 <- pdftext[[2]][100] %>% trimws()
rawtext123 <- pdftext[[2]][101] %>% trimws()
rawtext124 <- pdftext[[2]][102] %>% trimws()
rawtext125 <- pdftext[[2]][103] %>% trimws()
rawtext126 <- pdftext[[2]][104] %>% trimws()
rawtext127 <- pdftext[[2]][105] %>% trimws()
rawtext128 <- pdftext[[2]][106] %>% trimws()
rawtext129 <- pdftext[[2]][107] %>% trimws()
rawtext130 <- pdftext[[2]][108] %>% trimws()
rawtext131 <- pdftext[[2]][109] %>% trimws()
rawtext132 <- pdftext[[2]][110] %>% trimws()
rawtext133 <- pdftext[[2]][111] %>% trimws()
rawtext134 <- pdftext[[2]][112] %>% trimws()
rawtext135 <- pdftext[[2]][113] %>% trimws()
rawtext136 <- pdftext[[2]][114] %>% trimws()
rawtext137 <- pdftext[[2]][115] %>% trimws()
rawtext138 <- pdftext[[2]][116] %>% trimws()
rawtext139 <- pdftext[[2]][117] %>% trimws()
rawtext140 <- pdftext[[2]][118] %>% trimws()
rawtext141 <- pdftext[[2]][119] %>% trimws()
rawtext142 <- pdftext[[2]][120] %>% trimws()

# Dates and backup check
rawtext143 <- pdftext[[1]][2] %>% trimws()
rawtext144 <- pdftext[[2]][2] %>% trimws()

# Bind those into a one-column table
recent_crime_durham <- rbind(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50,rawtext51,rawtext52,rawtext53,rawtext54,rawtext55,rawtext56,rawtext57,rawtext58,rawtext59,rawtext60,rawtext61,rawtext62,rawtext63,rawtext64,rawtext65,rawtext66,rawtext67,rawtext68,rawtext69,rawtext70,rawtext71,rawtext72,rawtext73,rawtext74,rawtext75,rawtext76,rawtext77,rawtext78,rawtext79,rawtext80,rawtext81,rawtext82,rawtext83,rawtext84,rawtext85,rawtext86,rawtext87,rawtext88,rawtext89,rawtext90,rawtext91,rawtext92,rawtext93,rawtext94,rawtext95,rawtext96,rawtext97,rawtext98,rawtext99,rawtext100,rawtext101,rawtext102,rawtext103,rawtext104,rawtext105,rawtext106,rawtext107,rawtext108,rawtext109,rawtext110,rawtext111,rawtext112,rawtext113,rawtext114,rawtext115,rawtext116,rawtext117,rawtext118,rawtext119,rawtext120,rawtext121,rawtext122,rawtext123,rawtext124,rawtext125,rawtext126,rawtext127,rawtext128,rawtext129,rawtext130,rawtext131,rawtext132,rawtext133,rawtext134,rawtext135,rawtext136,rawtext137,rawtext138,rawtext139,rawtext140,rawtext141,rawtext142,rawtext143,rawtext144)
rm(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50,rawtext51,rawtext52,rawtext53,rawtext54,rawtext55,rawtext56,rawtext57,rawtext58,rawtext59,rawtext60,rawtext61,rawtext62,rawtext63,rawtext64,rawtext65,rawtext66,rawtext67,rawtext68,rawtext69,rawtext70,rawtext71,rawtext72,rawtext73,rawtext74,rawtext75,rawtext76,rawtext77,rawtext78,rawtext79,rawtext80,rawtext81,rawtext82,rawtext83,rawtext84,rawtext85,rawtext86,rawtext87,rawtext88,rawtext89,rawtext90,rawtext91,rawtext92,rawtext93,rawtext94,rawtext95,rawtext96,rawtext97,rawtext98,rawtext99,rawtext100,rawtext101,rawtext102,rawtext103,rawtext104,rawtext105,rawtext106,rawtext107,rawtext108,rawtext109,rawtext110,rawtext111,rawtext112,rawtext113,rawtext114,rawtext115,rawtext116,rawtext117,rawtext118,rawtext119,rawtext120,rawtext121,rawtext122,rawtext123,rawtext124,rawtext125,rawtext126,rawtext127,rawtext128,rawtext129,rawtext130,rawtext131,rawtext132,rawtext133,rawtext134,rawtext135,rawtext136,rawtext137,rawtext138,rawtext139,rawtext140,rawtext141,rawtext142,rawtext143,rawtext144)
recent_crime_durham <- as.data.frame(recent_crime_durham)

# name the column temporarily
names(recent_crime_durham) <- c("rawtext")
# remove the long white space on each end of each line
recent_crime_durham$rawtext2 <- strsplit(recent_crime_durham$rawtext, "\\s+\\s+")
# flatten the list this creates in processed column
recent_crime_durham <- recent_crime_durham %>% unnest_wider(rawtext2, names_sep = "_")

# get as of date out of this table now
asofdate <- recent_crime_durham[143,1]
asofdate <- mdy(gsub("Actual Offenses - Period Ending ","",asofdate))
# automating the updated date text in building tracker
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# name the columns temporarily
names(recent_crime_durham) = c("rawtext","category","this_month_so_far",
                               "last_month","total21","total22",
                               "change21to22","average18to22","ytd21",
                               "ytd22","ytd23","change22to23",
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
recent_crime_durham$ytd22 <- gsub(",","",recent_crime_durham$ytd22)
recent_crime_durham$ytd23 <- gsub(",","",recent_crime_durham$ytd23)
recent_crime_durham$total21 <- gsub(",","",recent_crime_durham$total21)
recent_crime_durham$total22 <- gsub(",","",recent_crime_durham$total22)

# convert the number cols to numeric
recent_crime_durham$ytd22 <- as.numeric(recent_crime_durham$ytd22)
recent_crime_durham$ytd23 <- as.numeric(recent_crime_durham$ytd23)
recent_crime_durham$total21 <- as.numeric(recent_crime_durham$total21)
recent_crime_durham$total22 <- as.numeric(recent_crime_durham$total22)

# remove NAs and replace with zeros if any exist
recent_crime_durham[is.na(recent_crime_durham)] <- 0

# export this file as RDS store for use in tracker building
saveRDS(recent_crime_durham,"scripts/rds/recent_crime_durham.RDS")