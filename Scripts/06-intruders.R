#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)


# breeding windows --------------------------------------------------------
#pull in litter table
litters <- tbl(con,"litter") %>%
  collect()

#ensure fieldBDate is in date format
litters$fieldBDate <- as.Date(litters$fieldBDate)

#calculate breeding windows; -35 days from earliest fieldBDate = start and -35 days from latest = end
breed_lac <- litters %>%
  filter(year(fieldBDate) >= 1987 & year(fieldBDate) <= 2023, ln == 1) %>%
  mutate(year = year(fieldBDate), grid = grid) %>%
  group_by(year, grid) %>%
  summarise(
    earliest_birth_date = min(fieldBDate),
    latest_birth_date = max(fieldBDate),
    breeding_start = earliest_birth_date - days(35),
    breeding_end = latest_birth_date + days(35),
    lac_start = earliest_birth_date + days(70),
    lac_end = latest_birth_date + days(70),
    .groups = 'drop')

#pull in census and trapping
spring_females <- read.csv("Input/spring_females.csv")
spring_males <- read.csv("Input/spring_males.csv")
trapping <- read.csv("Input/alltrapping.csv")

# sex ratios --------------------------------------------------------------
#count females and males per year in the spring census data
female_counts <- spring_females %>%
  mutate(year = year(date), gr = gr) %>%
  group_by(year, gr) %>%
  summarise(female_count = n(), .groups = 'drop')

male_counts <- spring_males %>%
  mutate(year = year(date), gr = gr) %>%
  group_by(year, gr) %>%
  summarise(male_count = n(), .groups = 'drop')

#merge the counts into a single dataframe and calculate the F:M ratio
sex_ratio <- female_counts %>%
  left_join(male_counts, by = c("year", "gr")) %>%
  mutate(sex_ratio = female_count / male_count) %>%
  rename(grid = gr)

#join this with the breed_lac table - keep only matches
breed_lac_ratio <- breed_lac %>%
  inner_join(sex_ratio, by = c("year", "grid"))

# intruders ----------------------------------------------------------------
#rename columns to organize
censusf <- spring_females %>%
  dplyr::select(squirrel_id, gr, locx, locy, date) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(-date) %>%
  mutate(sex = "F") %>%
  rename(grid = gr)
  

censusm <- spring_males %>%
  dplyr::select(squirrel_id, gr, locx, locy, date) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(-date) %>%
  mutate(sex = "M") %>%
  rename(grid = gr)

census <- bind_rows(censusf, censusm)

#perform comparison between trapping and census without joining tables together 
##but first.....
#prepare trapping data
trap <- trapping %>%
  mutate(
    year = year(date),  
    date_trap = date, 
    locx_trap = locx,
    locy_trap = locy,
    squirrel_id_trap = squirrel_id,
    sex_trap = sex,
    rep_con_trap = rep_con  # Assuming this column exists in your dataset
  ) %>%
  dplyr::select(-c(locx, locy, squirrel_id, sex, rep_con, date))

#reorder columns
trap <- trap %>%
  dplyr::select(year, grid, date_trap, squirrel_id_trap, sex_trap, locx_trap, locy_trap, rep_con_trap)

#prepare census data
census <- census %>%
  mutate(
    squirrel_id_owner = squirrel_id,
    sex_owner = sex,
    locx_owner = locx,
    locy_owner = locy) %>%
  dplyr::select(-c(squirrel_id, sex, locx, locy))

#reorder columns
census <- census %>%
  dplyr::select(year, grid, squirrel_id_owner, sex_owner, locx_owner, locy_owner) %>%
  mutate(locy_owner = as.numeric(as.character(locy_owner)))

# Perform the join
intruders <- trap %>%
  left_join(census, by = c("year", "grid", "locx_trap" = "locx_owner", "locy_trap" = "locy_owner")) %>%
  mutate(
    intruder = case_when(
      is.na(squirrel_id_owner) ~ NA_real_,  #no matching census entry, unclaimed midden
      squirrel_id_trap == squirrel_id_owner ~ 0,  #same squirrel, own midden, no intrusion
      TRUE ~ 1  #different squirrel, implies intrusion
    ))

table(intruders$intruder)

#remove NAs
intruders <- intruders %>%
  filter(!is.na(intruder))

# create 'season' column and add sex ratio --------------------------------------------------
intruders <- intruders %>%
  left_join(breed_lac, by = c("year", "grid")) %>%
  mutate(
    date_trap = as.Date(date_trap, format = "%Y-%m-%d"),
    season = case_when(
      date_trap >= breeding_start & date_trap <= breeding_end ~ "mating",
      date_trap >= lac_start & date_trap <= lac_end ~ "lactation",
      TRUE ~ "non-breeding")) 

#remove unwanted columns
intruders <- intruders %>%
  dplyr::select(
    -earliest_birth_date, -latest_birth_date,
    -breeding_start, -breeding_end,
    -lac_start, -lac_end)

#add sex ratio from breed_lac_ratio
intruders <- intruders %>%
  left_join(breed_lac_ratio %>% dplyr::select(year, grid, sex_ratio), by = c("year", "grid")) %>%
  na.omit()

# add snow column ---------------------------------------------------------
intruders <- intruders %>%
  mutate(
    snow = case_when(
      #snow period: October 15 to May 14
      (month(date_trap) %in% c(11, 12, 1, 2, 3, 4) | 
         (month(date_trap) == 10 & day(date_trap) >= 15) | 
         (month(date_trap) == 5 & day(date_trap) <= 14)) ~ "snow",
      
      #no-snow period: May 15 to October 14
      (month(date_trap) %in% c(6, 7, 8, 9) | 
         (month(date_trap) == 10 & day(date_trap) <= 14) | 
         (month(date_trap) == 5 & day(date_trap) >= 15)) ~ "no snow",
      
      #default: this should never trigger if the above cases are correct
      TRUE ~ "error"))

#save
write.csv(intruders, "Input/intruders.csv", row.names = FALSE)






