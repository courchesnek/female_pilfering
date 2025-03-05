#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# female spring census middens by year by grid ----------------------------
#start with 1984-2011 data
##pull in table
dbamidden <- tbl(con, "dbamidden") %>%
  collect()

#ensure date formatting
dbamidden$date <- as.Date(dbamidden$date, format = "%Y-%m-%d")

#filter for females and spring census
spring_F <- dbamidden %>%
  filter(Sex == "F", month(date) == 5) %>%
  dplyr::select(squirrel_id, grid, date, reflo, locX, locY)

#now for 2012-2023 data
##pull in table
census <- tbl(con, "census") %>%
  collect()

#ensure data formatting
census$census_date <- as.Date(census$census_date, format = "%Y-%m-%d")

#filter for females and spring census
spring_F2 <- census %>%
  filter(sex == "F", month(census_date) == 5) %>%
  dplyr::select(census_date, gr, locx, locy, reflo, squirrel_id)

#reorder columns
spring_F2 <- spring_F2 %>%
  dplyr::select(squirrel_id, gr, census_date, reflo, locx,  locy)

#rename columns to match before merge
spring_F <- spring_F %>%
  rename(gr = grid,
         locx = locX,
         locy = locY)

spring_F2 <- spring_F2 %>%
  rename(date = census_date)

#combine the two tables
spring_females <- bind_rows(spring_F, spring_F2)

#fix missing reflos
spring_females$reflo[spring_females$locx == "K.0" & spring_females$locy == 2.5 &
                     (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "K2."

spring_females$reflo[spring_females$locx == "1.5" & spring_females$locy == 9.3 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "A.9"

spring_females$reflo[spring_females$locx == "7.3" & spring_females$locy == 10.9 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "G10."

spring_females$reflo[spring_females$locx == "4.8" & spring_females$locy == 4.5 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "D.4."

spring_females$reflo[spring_females$locx == "14.1" & spring_females$locy == 12.3 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "N12"

spring_females$reflo[spring_females$locx == "A.0" & spring_females$locy == 5.5 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "A5."

spring_females$reflo[spring_females$locx == "0.0" & spring_females$locy == 1.5 &
                       (is.na(spring_females$reflo) | spring_females$reflo == "")] <- "01."

#remove NAs
spring_females <- na.omit(spring_females)

#fix weird reflos
spring_females$reflo[spring_females$reflo == "2"] <- "02."
spring_females$reflo[spring_females$reflo == "15"] <- "015"
spring_females$reflo[spring_females$reflo == "8"] <- "08"
spring_females$reflo[spring_females$reflo == "7"] <- "07"
spring_females$reflo[spring_females$reflo == "14"] <- "A4"
spring_females$reflo[spring_females$reflo == "14."] <- "A4."
spring_females$reflo[spring_females$reflo == "1.12"] <- "I.12"
spring_females$reflo[spring_females$reflo == "1.2"] <- "I.1."
spring_females$reflo[spring_females$reflo == "6.16"] <- "F.16."
spring_females$reflo[spring_females$reflo == "1.16"] <- "I.16"
spring_females$reflo[spring_females$reflo == "-.12"] <- "-0.12"
spring_females$reflo[spring_females$reflo == "-.3."] <- "-0.3."
spring_females$reflo[spring_females$reflo == "-.8."] <- "-0.8."
spring_females$reflo[spring_females$reflo == "-1"] <- "-1.0"
spring_females$reflo[spring_females$locx == "-1.0" & spring_females$locy == "0.0"] <- "-10"
spring_females$reflo[spring_females$reflo == "-114"] <- "-114."
spring_females$reflo[spring_females$reflo == "-12"] <- "-12."
spring_females$reflo[spring_females$locx == "-1.0" & spring_females$locy == "7.5"] <- "-17."
spring_females$reflo[spring_females$reflo == "-2"] <- "-2.0"
spring_females$reflo[spring_females$reflo == "-6"] <- "-6.0"
spring_females$reflo[spring_females$reflo == "0"] <- "00"
spring_females$reflo[spring_females$reflo == "0 0"] <- "00"
spring_females$reflo[spring_females$reflo == "0 17"] <- "017"
spring_females$reflo[spring_females$reflo == "0-0."] <- "0.-0"
spring_females$reflo[spring_females$locx == "0.5" & spring_females$locy == "-0.5"] <- "0.-0."
spring_females$reflo[spring_females$locx == "0.4" & spring_females$locy == "10.0"] <- "0.10"

# create new locx and locy columns ----------------------------------------
#split negatives from letters
negatives <- spring_females %>%
  filter(grepl("^-|^0", reflo))

#split letters from negatives
letters <- spring_females %>%
  filter(grepl("^[A-Za-z]", reflo))

#drop locx and locy columns
negatives <- negatives %>%
  dplyr::select(-locx, -locy)

letters <- letters %>%
  dplyr::select(-locx, -locy)

#split again based on dots for letters
letter_with_dot <- letters %>%
  filter(grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

letter_without_dot <- letters %>%
  filter(!grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

#make locx and locy columns
##with dot
letter_with_dot <- letter_with_dot %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]\\."),  #extract letter and decimal point
    locy = str_extract(reflo, "(?<=\\.)[0-9]*")  #remove locx part to get locy
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
      paste0(locy, ".5"),  #add .5 if decimal is in the 4th or 5th position of reflo
      paste0(locy, ".0")   #add .0 if no decimal in the 4th or 5th position of reflo
    )
  )

##fix weird locys
letter_with_dot$locy[letter_with_dot$reflo == "V.-0."] <- "-0.5"
letter_with_dot$locy[letter_with_dot$reflo == "U.-1"] <- "-1.0"
letter_with_dot$locy[letter_with_dot$reflo == "U.-0."] <- "-0.5"
letter_with_dot$reflo[letter_with_dot$squirrel_id == "24314"] <- "T.4."
letter_with_dot$locy[letter_with_dot$reflo == "T.4."] <- "4.5"
letter_with_dot$locy[letter_with_dot$reflo == "U.-0."] <- "-0.5"
letter_with_dot$locy[letter_with_dot$reflo == "P.-0"] <- "-0.0"
letter_with_dot$locy[letter_with_dot$reflo == "O.-0."] <- "-0.5"
letter_with_dot$locy[letter_with_dot$reflo == "O.-0"] <- "-0.0"
letter_with_dot$locy[letter_with_dot$reflo == "K.-0."] <- "-0.5"
letter_with_dot$reflo[letter_with_dot$reflo == "F.-.5"] <- "F.-0."
letter_with_dot$locy[letter_with_dot$reflo == "F.-0."] <- "-0.5"
letter_with_dot$reflo[letter_with_dot$reflo == "B.-.5"] <- "B.-0."
letter_with_dot$locy[letter_with_dot$reflo == "B.-0."] <- "-0.5"
letter_with_dot$reflo[letter_with_dot$squirrel_id == "24549"] <- "B.9."
letter_with_dot$locy[letter_with_dot$reflo == "B.9."] <- "9.5"
letter_with_dot$locy[letter_with_dot$reflo == "S.-0."] <- "-0.5"
letter_with_dot$locy[letter_with_dot$reflo == "Q.-0."] <- "-0.5"

##without dot 
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

#fix weird locys
letter_without_dot$reflo[letter_without_dot$squirrel_id == "3629"] <- "U-0."
letter_without_dot$reflo[letter_without_dot$squirrel_id == "5823"] <- "U-0."
letter_without_dot$locy[letter_without_dot$reflo == "U-0."] <- "-0.5"
letter_without_dot$reflo[letter_without_dot$reflo == "U-0.5"] <- "U-0."
letter_without_dot$locy[letter_without_dot$reflo == "U-0."] <- "-0.5"
letter_without_dot$reflo[letter_without_dot$reflo == "DD17"] <- "D17"
letter_without_dot$locy[letter_without_dot$reflo == "D17"] <- "17.0"
letter_without_dot$reflo[letter_without_dot$reflo == "DD16"] <- "D16"
letter_without_dot$locy[letter_without_dot$reflo == "D16"] <- "16.0"
letter_without_dot$reflo[letter_without_dot$reflo == "BB16"] <- "B16"
letter_without_dot$locy[letter_without_dot$reflo == "B16"] <- "16.0"
letter_without_dot$reflo[letter_without_dot$reflo == "BB16."] <- "B16."
letter_without_dot$locy[letter_without_dot$reflo == "B16."] <- "16.5"


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
###crap we need to remove 0s
zeros <- negatives_without_dot %>%
  filter(grepl("^0", reflo))  #filter for entries that begin with "0"
##we'll deal with these later

#remove the entries that begin with "0" from negatives_without_dot
negatives_without_dot <- negatives_without_dot %>%
  filter(!grepl("^0", reflo))  #keep entries that do not begin with "0"

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
    locx = case_when(reflo == "-1018" ~ "-10.0", TRUE ~ locx),
    locy = case_when(reflo == "-1018" ~ "18.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-102" ~ "-10.0", TRUE ~ locx),
    locy = case_when(reflo == "-102" ~ "2.0", TRUE ~ locy)
  )
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
    locx = case_when(reflo == "-1115" ~ "-11.0", TRUE ~ locx),
    locy = case_when(reflo == "-1115" ~ "15.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-1120" ~ "-11.0", TRUE ~ locx),
    locy = case_when(reflo == "-1120" ~ "20.0", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-1120." ~ "-11.0", TRUE ~ locx),
    locy = case_when(reflo == "-1120." ~ "20.5", TRUE ~ locy)
  )
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = case_when(reflo == "-1318" ~ "-13.0", TRUE ~ locx),
    locy = case_when(reflo == "-1318" ~ "18.0", TRUE ~ locy)
  )

#last but not least, the zeros...
##split based on dots
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

#fix weird locs
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = case_when(reflo == "0.-0." ~ "0.5", TRUE ~ locx),
    locy = case_when(reflo == "0.-0." ~ "-0.5", TRUE ~ locy)
  )
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = case_when(reflo == "0.-0" ~ "0.5", TRUE ~ locx),
    locy = case_when(reflo == "0.-0" ~ "-0.0", TRUE ~ locy)
  )


# FINALLYYYYYY let's combine all the subsections back together ------------
spring_females_clean <- bind_rows(
  letter_with_dot,
  letter_without_dot,
  negatives_with_dot,
  negatives_without_dot,
  zeros_with_dot,
  zeros_without_dot
)

write.csv(spring_females_clean, file = "Input/spring_females.csv", row.names = FALSE)

