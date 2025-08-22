#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in squirrel census and midden table
census <- tbl(con,"census") %>%
  collect()

census_middens <- tbl(con, "dbamidden") %>%
  collect()

#remove rows where locx, locy and reflo are all NA - can't do anything with those...
census <- census %>%
  filter(!(is.na(locx) & is.na(locy) & is.na(reflo)))

census_middens <- census_middens %>%
  filter(!(is.na(locX) & is.na(locY) & is.na(reflo)))

#clean up census data before merging
census <- census %>%
  mutate(census_date = as.Date(census_date))

census_spring <- census %>%
  mutate(year = year(census_date)) %>%
  filter(format(census_date, "%m-%d") == "05-15",
         gr %in% c("KL", "SU", "CH")) %>%
  dplyr::select(census_date, gr, squirrel_id, locx, locy, reflo)

census_middens <- census_middens %>%
  mutate(date = as.Date(date))

census_middens_spring <- census_middens %>%
  mutate(year = year(date)) %>%
  filter(format(date, "%m-%d") == "05-15",
         grid %in% c("KL", "SU", "CH")) %>%
  dplyr::select(date, grid, squirrel_id, locX, locY, reflo)

#remove rows where squirrel_id = NA - can't do anything with those
census_middens_spring <- census_middens_spring %>%
  filter(!is.na(squirrel_id))

# fix NA reflos ------------------------------------------------------
#2012-present
census_spring_missing_reflo <- census_spring %>%
  filter(is.na(reflo))

census_spring <- census_spring %>%
  mutate(reflo = case_when(
    is.na(reflo) & locx == "T.0" & locy == "1.0" ~ "T1",
    is.na(reflo) & locx == "J.5" & locy == "12" ~ "J.12",
    is.na(reflo) & locx == "D.0" & locy == "7.0" ~ "D7",
    is.na(reflo) & locx == "C.5" & locy == "17.0" ~ "C.17",
    is.na(reflo) & locx == "C.5" & locy == "15.5" ~ "C.15.",
    is.na(reflo) & locx == "B.5" & locy == "10.5" ~ "B.10.",
    is.na(reflo) & locx == "B.0" & locy == "2.0" ~ "B2",
    is.na(reflo) & locx == "A.5" & locy == "14.5" ~ "A.14.",
    is.na(reflo) & locx == "A.0" & locy == "10.5" ~ "A10.",
    is.na(reflo) & locx == "A.0" & locy == "3.5" ~ "A3.",
    is.na(reflo) & locx == "21.5" & locy == "2.7" ~ "U.2.",
    is.na(reflo) & locx == "17.0" & locy == "8.0" ~ "Q8",
    is.na(reflo) & locx == "14.8" & locy == "6.6" ~ "N.6.",
    is.na(reflo) & locx == "1.0" & locy == "7.0" ~ "A7",
    is.na(reflo) & locx == "0.0" & locy == "0.0" ~ "00",
    is.na(reflo) & locx == "0.0" & locy == "5.5" ~ "05.",
    is.na(reflo) & locx == "-8.5" & locy == "12.0" ~ "-8.12",
    is.na(reflo) & locx == "-6.4" & locy == "12.4" ~ "-612",
    is.na(reflo) & locx == "-6.0" & locy == "10.0" ~ "-610",
    is.na(reflo) & locx == "-5.0" & locy == "13.0" ~ "-513",
    is.na(reflo) & locx == "-5.0" & locy == "3.0" ~ "-53",
    is.na(reflo) & locx == "-4.0" & locy == "9.0" ~ "-49",
    is.na(reflo) & locx == "-3.0" & locy == "10.0" ~ "-310",
    is.na(reflo) & locx == "-2.0" & locy == "7.0" ~ "-27",
    is.na(reflo) & locx == "-2.0" & locy == "11.0" ~ "-211",
    is.na(reflo) & locx == "-1.0" & locy == "10.0" ~ "-110",
    is.na(reflo) & locx == "-1.0" & locy == "4.0" ~ "-14",
    is.na(reflo) & locx == "-1.0" & locy == "1.0" ~ "-11",
    is.na(reflo) & locx == "M.1" & locy == "1.8" ~ "M1.",
    is.na(reflo) & locx == "L.3" & locy == "5.7" ~ "L.3.",
    is.na(reflo) & locx == "C.0" & locy == "2.5" ~ "C2.",
    is.na(reflo) & locx == "K.5" & locy == "7.5" ~ "K.7.",
    is.na(reflo) & locx == "G.5" & locy == "1.0" ~ "G.1",
    is.na(reflo) & locx == "9.0" & locy == "3.0" ~ "I3",
    is.na(reflo) & locx == "8.0" & locy == "10.0" ~ "H10",
    is.na(reflo) & locx == "7.5" & locy == "10.9" ~ "G.11",
    is.na(reflo) & locx == "5.0" & locy == "15.0" ~ "E15",
    is.na(reflo) & locx == "19.0" & locy == "7.0" ~ "S7",
    is.na(reflo) & locx == "16.0" & locy == "10.0" ~ "P10",
    is.na(reflo) & locx == "16.0" & locy == "20.0" ~ "P20",
    is.na(reflo) & locx == "13.0" & locy == "11.0" ~ "M11",
    is.na(reflo) & locx == "13.0" & locy == "19.0" ~ "M19",
    is.na(reflo) & locx == "12.0" & locy == "5.0" ~ "L5",
    is.na(reflo) & locx == "10.5" & locy == "15.5" ~ "J.15.",
    is.na(reflo) & locx == "1.0" & locy == "2.0" ~ "A2",
    is.na(reflo) & locx == "0.0" & locy == "1.0" ~ "01",
    is.na(reflo) & locx == "0.0" & locy == "0.0" ~ "00",
    TRUE ~ reflo)) #keep existing reflo if none of the above conditions are met

#1987-2011
census_middens_spring_missing_reflo <- census_middens_spring %>%
  filter(is.na(reflo)) #no missing reflos!

# fix weird/missing locs/reflos -------------------------------------------
#missing reflos 2012-present
census_spring$reflo[census_spring$locx == "A.0" & census_spring$locy == 5.5 &
                       (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "A5."
census_spring$reflo[census_spring$locx == "K.5" & census_spring$locy == "8.0" &
                      (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "K.8"
census_spring$reflo[census_spring$locx == "K.0" & census_spring$locy == 2.5 &
                      (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "K2."
census_spring$reflo[census_spring$locx == "K.5" & census_spring$locy == "7.0" &
                      (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "K.7"
census_spring$reflo[census_spring$locx == "0.0" & census_spring$locy == 1.5 &
                      (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "01."
census_spring$reflo[census_spring$locx == "A.0" & census_spring$locy == 4.5 &
                      (is.na(census_spring$reflo) | census_spring$reflo == "")] <- "A4."

#weird reflos 2012-present
census_spring$reflo[census_spring$locx == "-0.4" & census_spring$locy == "13.6"] <- "-0.13."
census_spring$reflo[census_spring$locx == "-0.5" & census_spring$locy == "5.5"] <- "-0.5."
census_spring$reflo[census_spring$locx == "-1.2" & census_spring$locy == "14.5"] <- "-114."
census_spring$reflo[census_spring$locx == "-0.5" & census_spring$locy == "3.0"] <- "-0.3"
census_spring$reflo[census_spring$locx == "-0.8" & census_spring$locy == "2.8"] <- "-0.2."
census_spring$reflo[census_spring$locx == "-7.5" & census_spring$locy == "6.0"] <- "-7.6"
census_spring$reflo[census_spring$locx == "-1.2" & census_spring$locy == "14.5"] <- "-114."

census_spring$locx[census_spring$squirrel_id == 11945 & census_spring$reflo == "O5"] <- "O.0"
census_spring$locy[census_spring$squirrel_id == 11945 & census_spring$reflo == "O5"] <- "5.0"

#missing reflos 1987-2011
census_middens_spring$reflo[census_middens_spring$locX == "13.0" & census_middens_spring$locY == 9.5 &
                      (is.na(census_middens_spring$reflo) | census_middens_spring$reflo == "")] <- "M9."
census_middens_spring$reflo[census_middens_spring$locX == "7.5" & census_middens_spring$locY == 2.5 &
                              (is.na(census_middens_spring$reflo) | census_middens_spring$reflo == "")] <- "G.2."
census_middens_spring$reflo[census_middens_spring$locX == "1.5" & census_middens_spring$locY == 9.3 &
                              (is.na(census_middens_spring$reflo) | census_middens_spring$reflo == "")] <- "A.9"
census_middens_spring$reflo[census_middens_spring$locX == "7.3" & census_middens_spring$locY == 10.9 &
                              (is.na(census_middens_spring$reflo) | census_middens_spring$reflo == "")] <- "G.11"

# create new locx/locy columns 2012-present --------------------------------------------
#split into subtables
negatives <- census_spring %>%
  filter(grepl("^-", reflo)) %>%
  dplyr::select(-locx, -locy)

letters <- census_spring %>%
  filter(grepl("^[A-Za-z]", reflo)) %>%
  dplyr::select(-locx, -locy)

zeros <- census_spring %>%
  filter(grepl("^0", reflo)) %>%
  dplyr::select(-locx, -locy)

#letters
##split based on dots
letter_with_dot <- letters %>%
  filter(grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

letter_without_dot <- letters %>%
  filter(!grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

##letters with dots
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
    ))

#fix weird reflos/locs
letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "V.-0." & locx == "V.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "K.-0." & locx == "K.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "Q.-0." & locx == "Q.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "U.-0." & locx == "U.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "O.-0." & locx == "O.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "P.-0" & locx == "P.5", "-0.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "U.-1" & locx == "U.5", "-1.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "O.-0" & locx == "O.5", "-0.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(reflo == "B. 9.", "B.9.", reflo),
    locy = ifelse(reflo == "B.9." & squirrel_id == 24549, "9.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2020-05-15") & gr == "SU" & squirrel_id == 24314, "T.4.", reflo),
    locy = ifelse(census_date == as.Date("2020-05-15") & gr == "SU" & squirrel_id == 24314, "4.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "H4", reflo),
    locx = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "H.0", locx),
    locy = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "4.0", locy))

letter_with_dot$reflo[letter_with_dot$locx == "A.5" & letter_with_dot$locy == "12.5"] <- "A.12."

letter_with_dot$reflo[letter_with_dot$locx == "L.5" & letter_with_dot$locy == "3.5"] <- "L.3."

#now for letters without dots
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
    ))

#fix weird reflos/locs
letter_without_dot <- letter_without_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2012-05-15") & gr == "LL" & squirrel_id == 12372, "U-0.", reflo),
    locy = ifelse(census_date == as.Date("2012-05-15") & gr == "LL" & squirrel_id == 12372, "-0.5", locy))

letter_without_dot <- letter_without_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2022-05-15") & gr == "KL" & squirrel_id == 25335, "B2", reflo),
    locx = ifelse(census_date == as.Date("2022-05-15") & gr == "KL" & squirrel_id == 25335, "B.0", locx),
    locy = ifelse(census_date == as.Date("2022-05-15") & gr == "KL" & squirrel_id == 25335, "2.0", locy))

#split again based on dots for negatives
negatives_with_dot <- negatives %>%
  filter(grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #decimal in the third position

negatives_without_dot <- negatives %>%
  filter(!grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #no decimal in the third position

#make locx and locy columns 
##with dot 
##fix one weird reflo
negatives_with_dot <- negatives_with_dot %>%
  mutate(reflo = ifelse(reflo == "-7.5, -7.6", "-7.6", reflo))

negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locx = substr(reflo, 1, 3),  #extract the first three characters for locx
    locy = str_remove(reflo, "^.{3}"))  #remove the first three characters to get locy, keeping everything after

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
    ))

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
    ))

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
    ))

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
    ))

#fix weird reflos/locs
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = ifelse(reflo == "0.-0", "0.5", locx),
    locy = ifelse(reflo == "0.-0", "-0.0", locy))

# merge all tables back together
census_clean_new <- bind_rows(
  letter_with_dot,
  letter_without_dot,
  negatives_with_dot,
  negatives_without_dot,
  zeros_with_dot,
  zeros_without_dot)

# create new locx/locy columns 1987-2012 --------------------------------------------
#split into subtables
negatives_old <- census_middens_spring %>%
  filter(grepl("^-", reflo)) %>%
  dplyr::select(-locX, -locY)

letters_old <- census_middens_spring %>%
  filter(grepl("^[A-Za-z]", reflo)) %>%
  dplyr::select(-locX, -locY)

zeros_old <- census_middens_spring %>%
  filter(grepl("^0", reflo)) %>%
  dplyr::select(-locX, -locY)

#letters 
##split based on dots
letter_with_dot_old <- letters_old %>%
  filter(grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

letter_without_dot_old <- letters_old %>%
  filter(!grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

##letters with dots
#fix one weird reflo
letter_with_dot_old$reflo[letter_with_dot_old$reflo == "F.-.5"] <- "F.0."

#add locx/locy
letter_with_dot_old <- letter_with_dot_old %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]\\."),  #extract letter and decimal point
    locy = str_extract(reflo, "(?<=\\.)[0-9]+\\.?")  #remove locx part to get locy
  )

#add the .0's and .5's
##add .5 to locx
letter_with_dot_old <- letter_with_dot_old %>%
  mutate(locx = paste0(locx, "5"))

##add either .0 or .5 to locy
letter_with_dot_old <- letter_with_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(reflo) >= 4 & (substr(reflo, 4, 4) == "." | substr(reflo, 5, 5) == "."),
      paste0(locy, "5"),  #add .5 if decimal is in the 4th or 5th position of reflo
      paste0(locy, ".0")   #add .0 if no decimal in the 4th or 5th position of reflo
    ))

#now for letters without dots
letter_without_dot_old <- letter_without_dot_old %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]"),  #extract letter
    locy = str_remove(reflo, "^[A-Za-z]")  #remove the letter to get locy
  ) %>%
  mutate(locy = str_trim(locy))

#fix one weird reflo and locy
letter_without_dot_old$reflo[letter_without_dot_old$locx == "E.0" & letter_without_dot_old$locy == "-1.0"] <- "E1"
letter_without_dot_old$locy[letter_without_dot_old$locx == "E.0" & letter_without_dot_old$locy == "-1.0"] <- "1.0"

#add the .0's and .5's
##add .0 to locx
letter_without_dot_old <- letter_without_dot_old %>%
  mutate(locx = paste0(locx, ".0"))

##add either .0 or .5 to locy
letter_without_dot_old <- letter_without_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    ))

#split again based on dots for negatives 
negatives_with_dot_old <- negatives_old %>%
  filter(grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #decimal in the third position

negatives_without_dot_old <- negatives_old %>%
  filter(!grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #no decimal in the third position

#make locx and locy columns 
##with dot 
negatives_with_dot_old <- negatives_with_dot_old %>%
  mutate(
    locx = substr(reflo, 1, 3),  #extract the first three characters for locx
    locy = str_remove(reflo, "^.{3}")  #remove the first three characters to get locy, keeping everything after
  )

##add .5 to locx
negatives_with_dot_old <- negatives_with_dot_old %>%
  mutate(
    locx = paste0(locx, "5")  #add 5 after the decimal in locx
  )

##add either .0 or .5 to locy
negatives_with_dot_old <- negatives_with_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    ))

##without dot
#create locx and locy columns
negatives_without_dot_old <- negatives_without_dot_old %>%
  mutate(
    locx = str_extract(reflo, "^-\\d"),  #extract the negative sign and first digit
    locy = str_remove(reflo, "^-\\d")  #remove the locx part to get locy (everything after)
  )

##add .0 to locx
negatives_without_dot_old <- negatives_without_dot_old %>%
  mutate(locx = paste0(locx, ".0"))

negatives_without_dot_old <- negatives_without_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    ))

#split the zeros based on dots 
zeros_with_dot_old <- zeros_old %>%
  filter(grepl("^0\\.\\d", reflo) & !is.na(reflo))

zeros_without_dot_old <- zeros_old %>%
  filter(!grepl("^0\\.\\d", reflo) & !is.na(reflo))

#create locx and locy columns
zeros_with_dot_old <- zeros_with_dot_old %>%
  mutate(
    locx = str_extract(reflo, "^0\\."),
    locy = str_remove(reflo, "^0\\."))

#add .5 to locx
zeros_with_dot_old <- zeros_with_dot_old %>%
  mutate(
    locx = paste0(locx, "5"))

#add either .0 or .5 to locy
zeros_with_dot_old <- zeros_with_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    ))

#create locx and locy columns
zeros_without_dot_old <- zeros_without_dot_old %>%
  mutate(
    locx = str_extract(reflo, "^0") ,  #extract the "0" for locx
    locy = str_remove(reflo, "^0")  # Everything after "0" becomes locy
  )

#add .0 to locx
zeros_without_dot_old <- zeros_without_dot_old %>%
  mutate(
    locx = paste0(locx, ".0"))

#add either .0 or .5 to locy
zeros_without_dot_old <- zeros_without_dot_old %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    ))

# merge all tables back together
census_clean_old <- bind_rows(
  letter_with_dot_old,
  letter_without_dot_old,
  negatives_with_dot_old,
  negatives_without_dot_old,
  zeros_with_dot_old,
  zeros_without_dot_old) %>%
  rename(gr = grid, 
         census_date = date)

# merge old and new census tables together --------------------------------
census_clean <- bind_rows(census_clean_new, census_clean_old)

#save
write.csv(census_clean, "Output/census_clean.csv", row.names = FALSE)
