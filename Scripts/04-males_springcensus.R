#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# male spring census middens by year by grid ----------------------------
#start with 1984-2011 data
##pull in table
dbamidden <- tbl(con, "dbamidden") %>%
  collect()

#ensure date formatting
dbamidden$date <- as.Date(dbamidden$date, format = "%Y-%m-%d")

#filter for males and spring census
spring_M <- dbamidden %>%
  filter(Sex == "M", month(date) == 5) %>%
  dplyr::select(squirrel_id, grid, date, reflo, locX, locY)

#now for 2012-2023 data
##pull in table
census <- tbl(con, "census") %>%
  collect()

#ensure data formatting
census$census_date <- as.Date(census$census_date, format = "%Y-%m-%d")

#filter for females and spring census
spring_M2 <- census %>%
  filter(sex == "M", month(census_date) == 5) %>%
  dplyr::select(census_date, gr, locx, locy, reflo, squirrel_id)

#reorder columns
spring_M2 <- spring_M2 %>%
  dplyr::select(squirrel_id, gr, census_date, reflo, locx,  locy)

#rename columns to match before merge
spring_M <- spring_M %>%
  rename(gr = grid,
         locx = locX,
         locy = locY)

spring_M2 <- spring_M2 %>%
  rename(date = census_date)

#combine the two tables
spring_males <- bind_rows(spring_M, spring_M2) %>%
  na.omit()

spring_males$locx <- loc_to_numeric(spring_males$locx)

# fix missing reflos ----------------------------------------
spring_males$reflo[spring_males$squirrel_id == 8222 & 
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "139."

spring_males$reflo[spring_males$squirrel_id == 8594 & 
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "7.2."

spring_males$reflo[spring_males$squirrel_id == 22476 & 
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "11.8"

spring_males$reflo[spring_males$squirrel_id == 22229 & 
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "11.7"

spring_males$reflo[spring_males$squirrel_id == 22450 & 
                     spring_males$date == "2016-05-15" &
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "01."

spring_males$reflo[spring_males$squirrel_id == 12881 & 
                     (is.na(spring_males$reflo) | spring_males$reflo == "")] <- "14."


# fix weird reflos ----------------------------------------
spring_males$reflo[spring_males$reflo == "-.12"] <- "-0.12"
spring_males$reflo[spring_males$reflo == "-.3."] <- "-0.3."
spring_males$reflo[spring_males$reflo == "-.6."] <- "-0.6."
spring_males$reflo[spring_males$reflo == "-.8."] <- "-0.8."
spring_males$reflo[spring_males$reflo == "-1"] <- "-1.0"
spring_males$reflo[spring_males$reflo == "-102"] <- "-102."
spring_males$reflo[spring_males$reflo == "-114"] <- "-114."
spring_males$reflo[spring_males$squirrel_id == 8635] <- "-12."
spring_males$reflo[spring_males$locx == "-0.5" & spring_males$locy == "3.0"] <- "-0.3"
spring_males$reflo[spring_males$locx == "-2.0" & spring_males$locy == "2.5"] <- "-22."
spring_males$reflo[spring_males$locx == "-3.0" & spring_males$locy == "2.1"] <- "-32"
spring_males$reflo[spring_males$locx == "-1.0" & spring_males$locy == "3.0"] <- "-13"
spring_males$reflo[spring_males$squirrel_id == 8635 & spring_males$date == "2007-05-15"] <- "-14"
spring_males$reflo[spring_males$locx == "-1.0" & spring_males$locy == "3.0"] <- "-13"
spring_males$reflo[spring_males$locx == "-2" & spring_males$locy == "2.5"] <- "-22."
spring_males$reflo[spring_males$locx == "-3" & spring_males$locy == "2.1"] <- "-32"
spring_males$reflo[spring_males$locx == "-3.5" & spring_males$locy == "-0.3"] <- "-3.-0"
spring_males$reflo[spring_males$locx == "0.8" & spring_males$locy == "0.7"] <- "0.0."
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "0.0"] <- "00"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "0.4"] <- "00"
spring_males$reflo[spring_males$locx == "-7.5" & spring_males$locy == "6.0"] <- "-7.6"
spring_males$reflo[spring_males$locx == "-9.2" & spring_males$locy == "0.3"] <- "-90"
spring_males$reflo[spring_males$locx == "-9.6" & spring_males$locy == "1.4"] <- "-9.1"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "17.0"] <- "017"
spring_males$reflo[spring_males$locx == "-0.1" & spring_males$locy == "1.1"] <- "-01"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "1.1"] <- "01"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "10.0"] <- "010"
spring_males$reflo[spring_males$locx == "-0.3" & spring_males$locy == "10.1"] <- "-010"
spring_males$reflo[spring_males$locx == "0.1" & spring_males$locy == "14.9"] <- "015"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "15.0"] <- "015"
spring_males$reflo[spring_males$locx == "18.3" & spring_males$locy == "18.0"] <- "R18"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "4.7"] <- "04."
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "5.0"] <- "05"
spring_males$reflo[spring_males$locx == "0.1" & spring_males$locy == "5.5"] <- "05."
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "6.0"] <- "06"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "6.9"] <- "07"
spring_males$reflo[spring_males$locx == "0" & spring_males$locy == "9.0"] <- "09"
spring_males$reflo[spring_males$locx == "11.5" & spring_males$locy == "7.0"] <- "K.7"
spring_males$reflo[spring_males$locx == "11.5" & spring_males$locy == "8.0"] <- "K.8"
spring_males$reflo[spring_males$locx == "13" & spring_males$locy == "9.5"] <- "M9."
spring_males$reflo[spring_males$locx == "1" & spring_males$locy == "4.5"] <- "A4."
spring_males$reflo[spring_males$locx == "7.5" & spring_males$locy == "2.5"] <- "G.2."


# create new locx and locy columns ----------------------------------------
#split negatives from letters and drop locx/locy columns
negatives <- spring_males %>%
  filter(grepl("^-", reflo)) %>%
  dplyr::select(-locx, -locy)

#split letters from negatives
letters <- spring_males %>%
  filter(grepl("^[A-Za-z]", reflo)) %>%
  dplyr::select(-locx, -locy)

zeros <- spring_males %>%
  filter(grepl("^0", reflo)) %>%
  dplyr::select(-locx, -locy)

#split again based on dots for letters
letter_with_dot <- letters %>%
  filter(grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

letter_without_dot <- letters %>%
  filter(!grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

#add locx/locy
letter_with_dot <- letter_with_dot %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]\\."),  #extract letter and decimal point
    locy = str_extract(reflo, "(?<=\\.)[0-9]+\\.?")  #remove locx part to get locy
  )

#add the .0's and .5's
##add .5 to locx
letter_with_dot <- letter_with_dot %>%
  mutate(locx = paste0(locx, "5"))

##add either .0 or .5 to locy
letter_with_dot <- letter_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(reflo) >= 4 & (substr(reflo, 4, 4) == "." | substr(reflo, 5, 5) == "."),
      paste0(locy, "5"),  #add .5 if decimal is in the 4th or 5th position of reflo
      paste0(locy, ".0")   #add .0 if no decimal in the 4th or 5th position of reflo
    )
  )

#fix weird reflos/locs
letter_with_dot$reflo[letter_with_dot$reflo == "V.-.5"] <- "V.-0."
letter_with_dot$locy[letter_with_dot$reflo == "V.-0."] <- "-0.5"

#now letters without dots
letter_without_dot <- letter_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]"),  #extract letter
    locy = str_remove(reflo, "^[A-Za-z]")  #remove the letter to get locy
  ) %>%
  mutate(locy = str_trim(locy))

#add the .0's and .5's
##add .0 to locx
letter_without_dot <- letter_without_dot %>%
  mutate(locx = paste0(locx, ".0"))

##add either .0 or .5 to locy
letter_without_dot <- letter_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

#fix weird reflos/locs
letter_without_dot$reflo[letter_without_dot$squirrel_id == 2299] <- "U-0."
letter_without_dot$locy[letter_without_dot$reflo == "U-0."] <- "-0.5"

#split again based on dots for negatives
negatives_with_dot <- negatives %>%
  filter(grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #decimal in the third position

negatives_without_dot <- negatives %>%
  filter(!grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #no decimal in the third position

#make locx and locy columns 
##with dot 
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locx = substr(reflo, 1, 3),  #extract the first three characters for locx
    locy = str_remove(reflo, "^.{3}")  #remove the first three characters to get locy, keeping everything after
  )

##add .5 to locx
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locx = paste0(locx, "5")  #add 5 after the decimal in locx
  )

##add either .0 or .5 to locy
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

##without dot
#create locx and locy columns
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^-\\d"),  #extract the negative sign and first digit
    locy = str_remove(reflo, "^-\\d")  #remove the locx part to get locy (everything after)
  )

##add .0 to locx
negatives_without_dot <- negatives_without_dot %>%
  mutate(locx = paste0(locx, ".0"))

negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

#fix weird locs
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-102." ~ "-10.0", TRUE ~ locx),
    locy = case_when(reflo == "-102." ~ "2.5", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-108" ~ "-10.0", TRUE ~ locx),
    locy = case_when(reflo == "-108" ~ "8.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-1216" ~ "-12.0", TRUE ~ locx),
    locy = case_when(reflo == "-1216" ~ "16.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-1218" ~ "-12.0", TRUE ~ locx),
    locy = case_when(reflo == "-1218" ~ "18.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-3.-0" ~ "-3.5", TRUE ~ locx),
    locy = case_when(reflo == "-3.-0" ~ "-0.0", TRUE ~ locy)
  )

#split the zeros based on dots
zeros_with_dot <- zeros %>%
  filter(grepl("^0\\.\\d", reflo) & !is.na(reflo))

zeros_without_dot <- zeros %>%
  filter(!grepl("^0\\.\\d", reflo) & !is.na(reflo))

#create locx and locy columns
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locx = str_extract(reflo, "^0\\."),
    locy = str_remove(reflo, "^0\\."))

#add .5 to locx
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locx = paste0(locx, "5"))

#add either .0 or .5 to locy
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

#create locx and locy columns
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^0") ,  # Extract the "0" for locx
    locy = str_remove(reflo, "^0")  # Everything after "0" becomes locy
  )

#add .0 to locx
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = paste0(locx, ".0"))

#add either .0 or .5 to locy
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

# FINALLYYYYYY let's combine all the subsections back together ------------
spring_males_clean <- bind_rows(
  letter_with_dot,
  letter_without_dot,
  negatives_with_dot,
  negatives_without_dot,
  zeros_with_dot,
  zeros_without_dot
)

write.csv(spring_males_clean, file = "Input/spring_males.csv", row.names = FALSE)




