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
  dplyr::select(-earliest_birth_date, -latest_birth_date, -breeding_start, -breeding_end, -date)  #remove breeding window columns


# intruders ----------------------------------------------------------------
#rename columns to organize
censusf <- spring_females %>%
  dplyr::select(squirrel_id, locx, locy, date) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(-date) %>%
  mutate(sex = "F")
  

censusm <- spring_males %>%
  dplyr::select(squirrel_id, locx, locy, date) %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(-date) %>%
  mutate(sex = "M")

census <- bind_rows(censusf, censusm)

#perform comparison between trapping and census without joining tables together 
intruders <- filtered_trapping %>%
  left_join(census, by = c("year"), suffix = c("_trap", "_census"), relationship = "many-to-many") %>%
  mutate(
    intruder = case_when(
      #if the trapping locx and locy match the census locx and locy for the same squirrel and year
      locx_trap == locx_census & locy_trap == locy_census & squirrel_id_trap == squirrel_id_census ~ 0, #own midden, no intrusion
      
      locx_trap == locx_census & locy_trap == locy_census & squirrel_id_trap != squirrel_id_census ~ 1, #trapped on another squirrel's midden
      
      TRUE ~ NA_real_  #trapped on an un-claimed midden
    )
  ) %>%
  dplyr::select(-c(locx_census, locy_census))

table(intruders$intruder)

intruders <- intruders %>%
  filter(!is.na(intruder))

write.csv(intruders, "Output/intruders.csv", row.names = FALSE)

# sex-ratio analysis ------------------------------------------------------
sex_ratio_analysis_unfiltered <- intruders %>%
  #create a sex combination column based on sex_trap and sex_census
  mutate(
    sex_combination = case_when(
      sex_trap == "M" & sex_census == "M" ~ "Male on Male",
      sex_trap == "F" & sex_census == "F" ~ "Female on Female",
      sex_trap == "M" & sex_census == "F" ~ "Male on Female",
      sex_trap == "F" & sex_census == "M" ~ "Female on Male",
      TRUE ~ "Other"
    )
  ) %>%
  #group by sex_combination and calculate the count of intrusions for each group
  group_by(sex_combination) %>%
  summarise(
    intrusion_count = sum(intruder == 1, na.rm = TRUE),  #count intrusions (where intruder = 1)
    .groups = "drop"  # This ensures the data is ungrouped after summarisation
  ) %>%
  #add the total count of intrusions across all sex combinations
  mutate(
    total_count = sum(intrusion_count),  #total intrusions for all combinations
    intrusion_rate = intrusion_count / total_count  #calculate intrusion rate
  ) %>%
  #arrange the results for better readability
  arrange(sex_combination)

#save
write.csv(sex_ratio_analysis_unfiltered, "Output/sex_ratio_unfiltered.csv", row.names = FALSE)

#remove males on female middens as this is representative of mate searching, NOT an intent to pilfer
sex_ratio_analysis <- intruders %>%
  mutate(
    sex_combination = case_when(
      sex_trap == "M" & sex_census == "M" ~ "Male on Male",
      sex_trap == "F" & sex_census == "F" ~ "Female on Female",
      sex_trap == "M" & sex_census == "F" ~ "Male on Female",
      sex_trap == "F" & sex_census == "M" ~ "Female on Male",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!(sex_trap == "M" & sex_census == "F")) %>%  #exclude male on female middens (mating behavior)
  #group by sex_combination and calculate the count of intrusions for each group
  group_by(sex_combination) %>%
  summarise(
    intrusion_count = sum(intruder == 1, na.rm = TRUE),  #count intrusions (where intruder = 1)
    .groups = "drop"  #this ensures the data is ungrouped after summarisation
  ) %>%
  #add the total count of intrusions across all sex combinations
  mutate(
    total_count = sum(intrusion_count),  #total intrusions for all combinations
    intrusion_rate = intrusion_count / total_count  #calculate intrusion rate
  ) %>%
  #arrange the results for better readability
  arrange(sex_combination)

#save
write.csv(sex_ratio_analysis, "Output/sex_ratio.csv", row.names = FALSE)

# chi-square --------------------------------------------------------------

#create a sex_combination column in the intruders table
intruders_stats <- intruders %>%
  mutate(
    sex_combination = case_when(
      sex_trap == "M" & sex_census == "M" ~ "Male on Male",
      sex_trap == "F" & sex_census == "F" ~ "Female on Female",
      sex_trap == "M" & sex_census == "F" ~ "Male on Female",
      sex_trap == "F" & sex_census == "M" ~ "Female on Male",
      TRUE ~ "Other"
    )
  ) %>%
  filter(intruder == 1, sex_combination != "Male on Female")

#create a contingency table
contingency_table <- table(
  intruders_stats$sex_combination,  
  intruders_stats$intruder)

head(contingency_table)

#perform the chi-square test of independence
chisq_test <- chisq.test(contingency_table)

chisq_test

#get the chi-squared output
chi_square_statistic <- chisq_test$statistic
n <- sum(contingency_table) 
k <- nrow(contingency_table)

#calculate Cramér's V to measure the effect size
cramers_v <- sqrt(chi_square_statistic / (n * (k - 1)))

cramers_v


# plots -------------------------------------------------------------------
#total counts
# ggplot(sex_ratio_analysis, aes(x = sex_combination, y = intrusion_count, fill = sex_combination)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Intrusion Counts by Sex Combination",
#        x = "Sex Combination",
#        y = "Intrusion Count") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#rates
intrusion_rates <- ggplot(sex_ratio_analysis, aes(x = sex_combination, y = intrusion_rate, fill = sex_combination)) +
  geom_bar(stat = "identity") +
  labs(title = "Intrusion Rates by Sex Combination",
       x = "Sex Combination",
       y = "Intrusion Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("Output/intrusion_rates.jpeg", plot = intrusion_rates, width = 8, height = 6)

#proportions
ggplot(sex_ratio_analysis, aes(x = "", y = intrusion_count, fill = sex_combination)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Intrusions by Sex Combination") +
  theme_void()

