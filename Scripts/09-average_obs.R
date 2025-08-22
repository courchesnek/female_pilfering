#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in tables
behaviour <- tbl(con,"behaviour") %>%
  collect()

#only keep focals and cas obs
behaviour <- behaviour %>%
  filter(mode %in% c(1,3))

#study years
study_start <- 1987
study_end   <- 2024

all_years <- tibble(year = seq(study_start, study_end))

#average number of focals and cas obs per year
per_year <- behaviour %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year) %>%
  summarise(
    focals = sum(mode == 3, na.rm = TRUE),
    cas    = sum(mode == 1, na.rm = TRUE),
    .groups = "drop") %>%
  right_join(all_years, by = "year") %>%           # include years with no rows
  replace_na(list(focals = 0, cas = 0)) %>%        # turn missing into zeros
  mutate(total = focals + cas)

# Averages across ALL study years (zeros included)
averages <- per_year %>%
  summarise(
    avg_focals = mean(focals),
    avg_cas    = mean(cas),
    avg_total  = mean(total))
