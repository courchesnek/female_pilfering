#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))


# breeding windows --------------------------------------------------------
#pull in litter table
litters <- tbl(con,"litter") %>%
  collect()

#ensure fieldBDate is in date format
litters$fieldBDate <- as.Date(litters$fieldBDate)

#calculate breeding windows; -35 days from earliest fieldBDate = start and -35 days from latest = end
mating_lac <- litters %>%
  filter(year(fieldBDate) >= 1987 & year(fieldBDate) <= 2024, ln == 1) %>%
  mutate(year = year(fieldBDate), grid = grid) %>%
  group_by(year, grid) %>%
  summarise(
    earliest_birth_date = min(fieldBDate),
    latest_birth_date = max(fieldBDate),
    mating_start = earliest_birth_date - days(35),
    mating_end = latest_birth_date + days(35),
    lactation_start = earliest_birth_date + days(70),
    lactation_end = latest_birth_date + days(70),
    .groups = 'drop')

#pull in census and trapping ---------------------------------------------
census <- read.csv("Output/census_clean.csv")
trapping <- read.csv("Input/alltrapping.csv")

#add sex to census records -----------------------------------------------
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

#join squirrel sex info to census data and add year column from census_date
spring_census <- left_join(census, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE) %>%
  mutate(year = format(as.Date(census_date), "%Y"))

#reorder columns
spring_census <- spring_census %>%
  dplyr::select(census_date, year, gr, squirrel_id, sex, reflo, locx, locy)

# sex ratios --------------------------------------------------------------
#F:M per year (spring) per grid
sex_ratio <- spring_census %>%
  group_by(year, gr, sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sex, values_from = count, values_fill = list(count = 0)) %>%
  mutate(F_M = if_else(M > 0, F / M, NA_real_),
         year = as.numeric(year)) %>%
  rename(grid = gr)

#join this with the breed_lac table - keep only matches
mating_lac_ratio <- mating_lac %>%
  inner_join(sex_ratio, by = c("year", "grid"))

# intruders ----------------------------------------------------------------
#perform comparison between trapping and census without joining tables together 
##but first.....
#prepare trapping data
trap <- trapping %>%
  mutate(
    year = year(date),  #extract year from date
    date_trap = date, 
    locx_trap = locx,
    locy_trap = locy,
    squirrel_id_trap = squirrel_id,
    sex_trap = sex,
    rep_con_trap = rep_con) %>%
  dplyr::select(-c(locx, locy, squirrel_id, sex, rep_con, date)) %>% #remove unwanted columns
  dplyr::select(year, grid, date_trap, squirrel_id_trap, sex_trap, locx_trap, locy_trap, rep_con_trap) #reorder columns

#prepare census data
spring_census <- spring_census %>%
  mutate(
    squirrel_id_owner = squirrel_id,
    sex_owner = sex,
    locx_owner = locx,
    locy_owner = locy) %>%
  dplyr::select(-c(squirrel_id, sex, locx, locy)) %>%
  rename(grid = gr) %>%
  dplyr::select(year, grid, squirrel_id_owner, sex_owner, locx_owner, locy_owner) %>% #reorder columns
  mutate(locy_owner = as.numeric(locy_owner),
         year = as.numeric(year))

# Perform the join
intruders <- trap %>%
  left_join(spring_census, by = c("year", "grid", "locx_trap" = "locx_owner", "locy_trap" = "locy_owner")) %>%
  mutate(
    intruder = case_when(
      is.na(squirrel_id_owner) ~ NA_real_,  #no matching census entry, unclaimed midden
      squirrel_id_trap == squirrel_id_owner ~ 0,  #same squirrel, own midden, no intrusion
      TRUE ~ 1))  #different squirrel, implies intrusion

table(intruders$intruder)

#remove NAs
intruders <- intruders %>%
  filter(!is.na(intruder))

# create 'season' column and add sex ratio --------------------------------------------------
intruders <- intruders %>%
  left_join(mating_lac, by = c("year", "grid")) %>%
  mutate(
    date_trap = as.Date(date_trap, format = "%Y-%m-%d"),
    season = case_when(
      date_trap >= mating_start & date_trap <= mating_end ~ "mating",
      date_trap >= lactation_start & date_trap <= lactation_end ~ "lactation",
      TRUE ~ "non-breeding")) 

#remove unwanted columns
intruders <- intruders %>%
  dplyr::select(
    -earliest_birth_date, -latest_birth_date,
    -mating_start, -mating_end,
    -lactation_start, -lactation_end)

#add sex ratio from mating_lac_ratio
intruders <- intruders %>%
  left_join(mating_lac_ratio %>% dplyr::select(year, grid, F_M), by = c("year", "grid")) %>%
  na.omit()

#save
write.csv(intruders, "Input/intruders.csv", row.names = FALSE)
