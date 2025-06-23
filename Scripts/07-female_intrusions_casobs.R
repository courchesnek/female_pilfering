#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in feeding obs + census exact locs
feeding <- read.csv("Input/all_feeding_census_exact_locs.csv")

#pull in census data
census <- read.csv("Input/squirrel_census_exact_locs.csv")

#numeric locx to alphabet ----------------------------------
num_to_letter <- setNames(LETTERS, as.character(1:26))

numeric_to_locx <- function(x) {
  x_chr <- as.character(x)
  
  # 1) If it already starts with a letter, do nothing
  if (grepl("^[A-Za-z]", x_chr)) return(x_chr)
  
  # 2) If it's zero or negative, leave it untouched
  #    (we assume anything starting with "-" or "0" should stay)
  if (grepl("^0", x_chr) || grepl("^-", x_chr)) return(x_chr)
  
  # 3) Split into integer-part before dot and the rest
  base  <- sub("\\.(.*)$", "", x_chr)      # e.g. "3" from "3.57"
  after <- sub("^[^.]*", "", x_chr)        # e.g. ".57" from "3.57"
  
  # 4) Look up letter (only if base in 1:26)
  letter <- num_to_letter[base]
  if (is.na(letter)) {
    warning("Integer part out of A–Z range (or not 1–26): ", base)
    return(x_chr)}
  
  # 5) Reassemble
  paste0(letter, after)}

numeric_to_locx_vec <- Vectorize(numeric_to_locx)

feeding_mating <- feeding %>%
  mutate(locx_feed = numeric_to_locx_vec(locx_feed))

#filter for mating repro stage only --------------
feeding_mating <- feeding_mating %>%
  filter(repro_stage == "mating")

#filter for cached (cones and mushrooms) resoures only --------------
feeding_mating <- feeding_mating %>%
  filter(food_type == "capital")

#identify female intrusions ----------------------------------------------
#first, add sex to census table and filter for May census 
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

census <- left_join(census, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE)

census <- census %>%
  mutate(census_date = as.Date(census_date)) %>%
  filter(format(census_date, "%m-%d") == "05-15")

#create a year column
census <- census %>%
  mutate(year = as.numeric(format(census_date, "%Y")))

#locx back to numeric in both tables
feeding_mating$locx_feed <- loc_to_numeric(feeding_mating$locx_feed)

census$locx <- loc_to_numeric(census$locx)

#okay now, identify intrusions
intrusions <- feeding_mating %>%
  rename(
    feeder_id   = squirrel_id,
    feeder_sex  = sex) %>%
  inner_join(
    census %>%
      rename(
        midden_owner = squirrel_id,
        locx_mid     = locx,
        locy_mid     = locy,
        midden_owner_sex  = sex) %>%
      # make sure these are numeric
      mutate(
        locx_mid = as.numeric(locx_mid),
        locy_mid = as.numeric(locy_mid)),
    by = c("grid","year"),
    relationship = "many-to-many") %>%
  # also coerce the feeds
  mutate(
    locx_feed = as.numeric(locx_feed),
    locy_feed = as.numeric(locy_feed),
    
    dx = abs(locx_feed - locx_mid), #distance between feeding loc and midden loc
    dy = abs(locy_feed - locy_mid), #distance between feeding loc and midden loc
    
    on_midden  = (dx <= 0.3 & dy <= 0.3), #on midden = within 10m
    off_midden = (dx >= 0.4 | dy >= 0.4), #off midden = outside of 10m
    #intruder = feeding off her midden and on a midden owned by a different squirrel (within 9m of another midden -> 0.3)
    intruder   = (!on_midden & off_midden & feeder_id != midden_owner)) %>%
  dplyr::select(
    grid, year,
    feeder_id, feeder_sex,
    locx_feed, locy_feed,
    midden_owner, midden_owner_sex,
    locx_mid,   locy_mid,
    dx, dy,
    on_midden, off_midden, intruder)

#filter for female intruders only
intrusions <- intrusions %>%
  filter(feeder_sex == "F",
         intruder == TRUE)

# intrusions contains only:
# - feeding events off her own midden
# - on another midden that’s in the census for that grid & year
  
  
  
  
