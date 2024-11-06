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
breeding_window <- litters %>%
  filter(year(fieldBDate) >= 1987 & year(fieldBDate) <= 2023, ln ==1) %>%
  group_by(year = year(fieldBDate)) %>%
  summarise(
    earliest_birth_date = min(fieldBDate),
    latest_birth_date = max(fieldBDate),
    breeding_start = min(fieldBDate) - days(35),
    breeding_end = max(fieldBDate) - days(35)
  )

#pull in census and trapping
spring_females <- read.csv("Input/spring_females.csv")
spring_males <- read.csv("Input/spring_males.csv")
trapping <- read.csv("Input/alltrapping.csv")


# filter trapping records to within breeding windows ----------------------
#ensure date is in date format
trapping$date <- as.Date(trapping$date)

#filter trapping records for only those within corresponding breeding window
trapping <- trapping %>%
  mutate(year = year(date))

filtered_trapping <- trapping %>%
  left_join(breeding_window, by = "year") %>%  #join on the year
  filter(date >= breeding_start & date <= breeding_end) %>%  #filter for records within the breeding window
  filter(sex == "F")
  dplyr::select(-earliest_birth_date, -latest_birth_date, -breeding_start, -breeding_end)  #remove breeding window columns


# intruders ----------------------------------------------------------------
#rename columns to organize
censusf <- spring_females %>%
  rename(censusflocx = locx,
         censusflocy = locy) %>%
  dplyr::select(squirrel_id, censusflocx, censusflocy)

censusm <- spring_males %>%
  rename(censusmlocx = locx,
         censusmlocy = locy) %>%
  dplyr::select(squirrel_id, censusmlocx, censusmlocy)

intruders <- filtered_trapping %>%
  left_join(censusf, by = "squirrel_id", suffix = c("", "_f"), relationship = "many-to-many") %>%  #merge with female census data by squirrel_id
  left_join(censusm, by = "squirrel_id", suffix = c("", "_m"), relationship = "many-to-many") %>%  #merge with male census data by squirrel_id
  mutate(
    intruder = case_when(
      locx == censusflocx & locy == censusflocy ~ 0,  #trapped on her own midden
      locx == censusmlocx & locy == censusmlocy ~ 1,  #trapped on a male's midden
      TRUE ~ NA_real_  #no match found
    )
  ) %>%
  dplyr::select(-starts_with("censusm"))  #remove male-related columns

sum(is.na(intruders$intruder))

intruders <- intruders %>%
  filter(!is.na(intruder))


# summary -----------------------------------------------------------------
summary_stats <- intruders %>%
  group_by(intruder) %>%
  summarise(
    count = n(),  #count of occurrences
    proportion = n() / nrow(intruders) * 100  #proportion as a percentage
  )


# data distribution -------------------------------------------------------
ggplot(intruders, aes(x = factor(intruder))) +
  geom_bar(fill = "skyblue", color = "black") +
  scale_x_discrete(labels = c("0" = "Own Midden", "1" = "Male Midden")) +
  labs(
    x = "Trapping Location",
    y = "Count of Female Trapping Events"
  ) +
  theme_minimal()















