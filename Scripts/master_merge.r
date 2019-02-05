# MASTER REIS DATASET MERGE
  
  ## RStudio Version: 1.1.456
  ## R Version: 3.5.1
  ## Windows 10

  ## Script Version: 1.0
  ## Updated: 2019-01-16


# CLEAR WORKSPACE; INSTALL/LOAD PACKAGES

rm(list = ls())

if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(purrr)){install.packages("purrr")}
if(!require(stringr)){install.packages("stringr")}
if(!require(lubridate)){install.packages("lubridate")}

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)


# PASTE URLS; READ IN DATA: LAKEVIEW AMPHITHEATER

prefix <- "https://raw.githubusercontent.com/jamisoncrawford/lakeview/master/Final%20Tables/tblr_"
suffix <- ".csv"

name1 <- c("ajay_glass",     "allied_electric",   "am_electric",        "atlas_fence", "benedict_floor",
           "bh_piping",      "burn_bros",         "cny_sheet_metal",    "davis_ulmer", "ej_construction",
           "eugene_sackett", "evan_corporation",  "herbert_darling",    "john_lowery",
           "kc_masonry",     "ksp_painting",      "maxim_construction", "murnane_construction",
           "northeast",      "oconnell_electric", "postler_jaeckle",    "quality_structures_lv",
           "rommel_fence",   "seneca_steel",      "syracuse_scenery",   "tradesmen_international",
           "watson_farms",   "will_cvp")

type <- "ccDcccddddcciiiiii"

urls <- paste0(prefix, name1, suffix); rm(prefix, suffix)

for (i in seq_along(name1)){
  assign(name1[i], read_csv(file = urls[i], col_types = type))
}


# PASTE URLS: READ IN DATA: HANCOCK AIRPORT

prefix <- "https://raw.githubusercontent.com/jamisoncrawford/hancock/master/Final%20Tables/tblr_"
suffix <- ".csv"

name2 <- c("danforth", "environmental",      "longhouse",      "ms_fire",     "niagara",
           "patricia", "quality_structures", "schalk_and_son", "stone_bridge")

type <- "ccDcccddddcciiiiii"

urls <- paste0(prefix, name2, suffix); rm(prefix, suffix)

for (i in seq_along(name2)){
  assign(name2[i], read_csv(file = urls[i], col_types = type))
}


# MERGE: LAKEVIEW & HANCOCK

names <- c(name1, name2); rm(name1, name2)

master <- do.call("bind_rows", mget(names))

rm(list = setdiff(ls(), "master"))


# PAD VARIABLES "SSN" & "ZIP"

names(master)
unique(master$class)
table(master$race)

master$ssn <- str_pad(string = master$ssn, width = 4, 
                      side = "left", pad = "0")

master$zip <- str_pad(string = master$zip, width = 5, 
                      side = "left", pad = "0")


# UNIFORM VARIABLE: "RACE"

table(master$race)

master$race <- str_replace_all(master$race, "American Indian or Alaskan|Native American", "Native")
master$race <- str_replace_all(master$race, "Black or African American", "Black")
master$race <- str_replace_all(master$race, "Two or More Races", "Multiracial")


# CHECK VARIABLE: "ENDING"; UNIFORM VALUES (CEILING)

end <- unique(master$ending)
master[which(master$ending == "1936-02-29"), "ending"] <- date("2018-04-29")

master$ending <- ceiling_date(master$ending, unit = "week")
rm(end)


# CLEAN VARIABLE "CLASS" LEVEL: "APPRENTICE"

unique(master$class)

app <- c("LAP3", "Apprentice",            "2nd Period Apprentice",       "PL/FTR 4THYR SYR",
         "PL/FTR 1ST YR",                 "PLB/FTR 2NDYR SY",            "PL/FTR 5THYR SYR",
         "Sheet Metal Apprentice",        "Sprinkler Fitter Apprentice",
         "Tile Mechanic Appr Year 4 1/2", "Tile Finisher Appt Year 1",
         "4th Yr Appr Piledriver",        "Apprentice Carpenter",        "Apprentice Taper",
         "Apprentice 4th Per",            "Apprentice 5th Per",          "Apprentice 2nd Per",
         "Apprentice 6th Per",            "Apprentice 3rd Per",          "Apprentice 1st Per",
         "Plumber Apprentice",            "Apprentice 1st Year",         "Apprentice 3rd Year",
         "Apprentice 2nd Year",           "IW Apprentice",               "Ironworker Apprentice", 
         "2nd Year Apprentice",           "3rd Period Apprentice",       "4th Period Apprentice")

index <- which(master$class %in% app)

master[index, "class"] <- "Apprentice"

rm(app)


# CLEAN VARIABLE "CLASS" LEVEL: "JOURNEYMAN"

unique(master$class)

jrn <- c("Iron Workers Rochester",   "Syracuse Iron Workers",       "Syracuse Glazers", 
         "LMJ1",                     "Journeyman",                  "Journey Wireman", 
         "Ironworker",               "Cement Mason",                "Piping Journeyman", 
         "PLBR/FTR JOUR SY",         "Sprinkler Fitter Journeyman", "Tile Finisher Journeyman", 
         "Tile Mechanic Journeyman", "Piledriver Journeyman",       "Piledriver Welder", 
         "Carpenter",                "Mason",                       "Lab Haz 1", 
         "Journeyman Techn",         "Journeyman 2nd S",            "Plumber Journeyman", 
         "Service Tradesman",        "Iron Worker",                 "IWJ", 
         "Oiler",                    "Journeyman Iron Worker",      "IW Journeyman",
         "Painter",                  "Taper")

index <- which(master$class %in% jrn)

master[index, "class"] <- "Journeyman"

rm(jrn)


# CLEAN VARIABLE "CLASS" LEVEL: "FOREMAN"

unique(master$class)

frm <- c("Sub-Foreman",        "Foreman Laborer",   "PL/FTR FORE SYRA",
         "Foreman 2",          "Foreman",           "Piledriver Foreman",
         "Piledriver Foreman", "Foreman A",         "IWF",
         "Subforeman",         "Carpenter Foreman", "Plumber Foreman",
         "Lab. Form. Base")

index <- which(master$class %in% frm)

master[index, "class"] <- "Foreman"

rm(frm)


# CLEAN VARIABLE "CLASS" LEVEL: "GENERAL FOREMAN"

unique(master$class)

gen <- c("PL/FTR GFORE SYR",  "General Foreman", "Supervisor",
         "Superintendent GF", "Superintendent")

index <- which(master$class %in% gen)

master[index, "class"] <- "General Foreman"

rm(gen)


# CLEAN VARIABLE "CLASS" LEVEL: "OPERATOR"

unique(master$class)

opr <- c("Operator",                 "Operating Engineer",  "Crane Op PD 100",
         "A-Operator",               "Crane Op PD 150",     "Crane Op Lattice",
         "Crane Operator",           "O. Eng.",             "Operator Engineer",
         "Engineer",                 "Operator Class 3",    "Operator Class 2",
         "Operator - Master Mechan", "Chief Equi Mechanic", "H50",
         "Operator Class 1",         "Operator Class A",    "HH OEA")

index <- which(master$class %in% opr)

master[index, "class"] <- "Operator"

rm(opr)


# CLEAN VARIABLE "CLASS" LEVEL: "OFFICE"

unique(master$class)

adm <- c("Admin", "Field Office Staff", "Office Staff", "Office", "Clerical")

index <- which(master$class %in% adm)

master[index, "class"] <- "Office"

rm(adm)


# CLEAN VARIABLE "CLASS" LEVEL: "LABORER"

unique(master$class)

lbr <- c("Laborer", "Labor", "Laborer Base", "Building Laborer")

index <- which(master$class %in% lbr)

master[index, "class"] <- "Laborer"

rm(lbr)


# CLEAN VARIABLE "CLASS" LEVEL: ALL OTHER

unique(master$class)

oth <- c("Owner", "Teamster Truck Driver")

index <- which(master$class %in% oth)

master[index, "class"] <- "Other"

rm(oth, index)


# INSPECT & TREAT DUPLICATE RECORDS

## M & S Fire Protection

for (i in 1:nrow(master)){
  if (master$name[i] == "M&S Fire Protection" & master$dup[i] == 1){
    master[i, ] <- NA
  }
}

## Environmental Services

for (i in 1:nrow(master)){
  if (master$name[i] == "Environmental" & master$dup[i] == 1 & !is.na(master$dup[i])){
    master$dup[i] <- 0 
  }
}

## Watson Farms

index <- which(master$dup == 1 & master$name == "Watson Farms")
master[index, "dup"] <- 0

## John Lowery

index <- which(master$dup == 1 & master$name == "John Lowery")
master[index, "dup"] <- 0

## AM Electric

index <- which(master$name == "AM Electric" & master$dup == 1)
master <- master[-index, ]

rm(i, index)


# DETECT & TREAT UNRECORDED DUPLICATES

dup <- master %>% 
  select(project:ssn) %>%
  duplicated() %>%
  as_tibble() %>%
  rename(dup = value)

master$dup <- as.logical(master$dup)

master <- master %>%
  select(-dup, -add)

master <- bind_cols(master, dup); rm(dup)

index <- which(master$dup == TRUE)
master <- master[-index, ]

master <- master %>% 
  select(-dup)

rm(index)


# WRITE TO .CSV

write_csv(master, "tblr_master.csv")
