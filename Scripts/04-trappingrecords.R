#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in trapping records; dbatrapping = 1984-2012 & trapping = 2013-2024
dbatrapping <- tbl(con,"dbatrapping") %>%
  collect()

trapping <- tbl(con,"trapping") %>%
  collect()

# fix dbatrapping ---------------------------------------------------------
#remove unnecessary columns and NAs
dbatrapping <- dbatrapping %>%
  dplyr::select(squirrel_id, gr, date, locX, locY, Sex, BrStatus) %>%
  na.omit()

dbatrapping_fixed <- dbatrapping

#once again, let's fix loc's
##function to round to .0 or 5
round_values <- function(value) {
  # Convert value to numeric
  numeric_value <- as.numeric(value)
  
  # Apply rounding rules
  rounded_value <- case_when(
    numeric_value %% 1 >= 0.1 & numeric_value %% 1 <= 0.3 ~ floor(numeric_value),  # Round down to the nearest whole number
    numeric_value %% 1 >= 0.4 & numeric_value %% 1 <= 0.8 ~ floor(numeric_value) + 0.5,  # Round to the nearest .5
    numeric_value %% 1 == 0.9 ~ ceiling(numeric_value),  # Round up to the next whole number
    TRUE ~ numeric_value  # Keep as is for other values
  )
  
  # Return the rounded values as a string with correct formatting
  formatted_value <- ifelse(rounded_value %% 1 == 0, 
                            paste0(rounded_value, ".0"), 
                            paste0(floor(rounded_value), ".5"))  # Ensure .5 is added correctly
  
  return(formatted_value)
}

#apply the function
dbatrapping_fixed$locX <- sapply(dbatrapping_fixed$locX, round_values)
dbatrapping_fixed$locY <- sapply(dbatrapping_fixed$locY, round_values)

##numeric locx to alphabet
numeric_to_alphabet <- function(numeric_value) {
  #convert the input to numeric for calculations
  num_value <- as.numeric(numeric_value)
  
  if (is.na(num_value) || num_value < 1) {
    return(as.character(numeric_value))  #return negative numbers and zeros unchanged
  } else {
    #split into integer and decimal parts
    integer_part <- floor(num_value)  #get the integer part
    decimal_part <- num_value - integer_part  #get the decimal part
    
    #convert integer part to corresponding letter
    alphabet_part <- LETTERS[integer_part]
    
    #format the decimal part correctly
    if (decimal_part == 0) {
      return(paste0(alphabet_part, ".0"))  #append .0 if there is no decimal
    } else {
      return(paste0(alphabet_part, ".", round(decimal_part * 10)))  #append the rounded decimal
    }
  }
}

dbatrapping_fixed$locX <- sapply(dbatrapping_fixed$locX, numeric_to_alphabet)

dbatrapping_fixed <- dbatrapping_fixed %>%
  rename(locx = locX, 
         locy = locY,
         sex = Sex,
         rep_con = BrStatus)

# fix trapping ------------------------------------------------------------
trapping <- trapping %>%
  dplyr::select(squirrel_id, gr, date, locx, locy, sex, rep_con) %>%
  na.omit()

trapping_fixed <- trapping
trapping_fixed$locx <- loc_to_numeric(trapping_fixed$locx)

#fix loc's
##apply function to round to .0 or .5
trapping_fixed$locx <- sapply(trapping_fixed$locx, round_values)
trapping_fixed$locy <- sapply(trapping_fixed$locy, round_values)

trapping_fixed <- trapping_fixed %>%
  na.omit()

trapping_fixed$locx <- sapply(trapping_fixed$locx, numeric_to_alphabet)

# merge all years together ------------------------------------------------
alltrapping <- bind_rows(dbatrapping_fixed, trapping_fixed) %>%
  rename(grid = gr)

#save
write.csv(alltrapping, file = "Input/alltrapping.csv", row.names = FALSE)