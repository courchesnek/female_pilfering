#load packages
source("Scripts/00-packages.R")

#connection to KRSP databases
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password")
)

#pull in tables
##the supplementary tables are not updated in the annual data cleanup, so squirrel_id values must be updated from the historic_squirrel_ids table
historic_ids<- tbl(con, "historic_squirrel_ids") %>% 
  dplyr::select(old_squirrel_id, new_squirrel_id) %>% 
  collect()

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL", "CH", "AG", "LL", "JO", "RR", "SX")) %>% 
  dplyr::select(squirrel_id, sex, byear) %>% 
  collect()

midden_cones <-tbl(con_suppl, "midden_cones") %>% 
  filter(squirrel_id !="UTS") %>% #remove UTS squirrels from data
  collect() %>% 
  left_join (historic_ids, by=c("squirrel_id" = "old_squirrel_id")) %>% 
  mutate (squirrel_id = ifelse(is.na(new_squirrel_id), squirrel_id, new_squirrel_id),
          date = as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S')) # date looks to be in datetime format

midden_cones <- midden_cones %>% 
  mutate (squirrel_id = ifelse(squirrel_id == 19851, 19537, squirrel_id),
          squirrel_id = ifelse(squirrel_id == 19911, 11895, squirrel_id))


# calculate cache sizes ---------------------------------------------------
midden_cones <- midden_cones %>%   
  mutate(total_new_2019 = total_newclosed + total_newopen,
         total_new = coalesce(total_new, total_new_2019),
         total_cones = total_old + total_new,
         total_cones2 = total_old + total_newclosed,
         cache_size_total = (total_cones / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_total2 = (total_cones2 / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new = (total_new / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_closed = (total_newclosed / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_open = (total_newopen / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_old = (total_old / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         squirrel_id = as.numeric(as.character(squirrel_id))) %>%  # Needed to match variable types.  The as.character is needed otherwise the squirrel_ids get recoded as 1...n
  group_by(squirrel_id, year) %>% # This line of code and the one below removes cases where a squirrel owns more than one midden
  slice(which.max(cache_size_total)) %>% # keeps the midden with more cones
  dplyr::select(year, grid, midden, date, locx, locy, squirrel_id, cache_size_total, cache_size_total2, cache_size_new_open, cache_size_new_closed, cache_size_new, cache_size_old) 

midden_cones <- midden_cones %>% 
  left_join(flastall, by="squirrel_id")

midden_cones <- midden_cones %>%
  dplyr::select(-date, -cache_size_total2, -cache_size_new_open, -cache_size_new_closed, -byear, -cache_size_total, -cache_size_old)



# combine cache and intrusion data ----------------------------------------------------------
intruders <- read.csv("Output/intruders.csv")

##add row_id to the original intruders table to preserve the order
intruders <- intruders %>%
  mutate(row_id = row_number())

#split intruders into trapped squirrels and census squirrels
trapped_squirrels <- intruders %>%
  dplyr::select(row_id, year, gr, squirrel_id_trap, sex_trap, locx_trap, locy_trap, intruder)

midden_owners <- intruders %>%
  dplyr::select(row_id, year, gr, squirrel_id_census, sex_census)

#add in cache data
trapped_squirrels_cache <- merge(trapped_squirrels, midden_cones[, c("year", "squirrel_id", "cache_size_new")], 
                                 by.x = c("squirrel_id_trap", "year"), by.y = c("squirrel_id", "year"), 
                                 all.x = TRUE) %>%
                           na.omit()

midden_owners_cache <- merge(midden_owners, midden_cones[, c("year", "squirrel_id", "cache_size_new")], 
                             by.x = c("squirrel_id_census", "year"), by.y = c("squirrel_id", "year"), 
                             all.x = TRUE) %>%
                       na.omit()

#merge the two tables back together, making sure the right trapping events are going with the right census records
cache_intruder <- merge(trapped_squirrels_cache, midden_owners_cache, by = "row_id", all.x = TRUE) %>%
  na.omit()

#clean up the final table
cache_intruder <- cache_intruder %>%
  dplyr::select(-row_id) %>%
  rename(
    cache_size_new_trap = cache_size_new.x,
    cache_size_new_census = cache_size_new.y,
    year = year.x, 
    grid = gr.x
  ) %>%
  dplyr::select(-year.y, -gr.y) %>%
  dplyr::select(year, grid, everything())

# males whose middens were never intruded on vs intruded on ----------------------------------------
midden_intrusion_summary <- cache_intruder %>%
  group_by(squirrel_id_census, year, sex_census) %>%
  summarise(
    all_unintruded = all(intruder == 0),
    any_intruded = any(intruder == 1)  
  ) %>%
  ungroup() #it's always good practice to ungroup after summarize if further manipulations are needed


cache_intrusions <- cache_intruder %>%
  left_join(midden_intrusion_summary, by = c("squirrel_id_census", "year", "sex_census")) %>%
  dplyr::select(-any_intruded)

  
# data distribution & transformation -------------------------------------------------------
##histogram
ggplot(cache_intrusions, aes(x = cache_size_new_census)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Cache Sizes", x = "Cache Size", y = "Frequency")

  
#NEED TO CONTROL FOR MAST YEAR!!!
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)

cache_intrusions <- cache_intrusions %>%
  mutate(MAST = year %in% mast_years)

cache_intrusions <- cache_intrusions %>%
  group_by(MAST) %>%
  mutate(
    log_cache_census = log(cache_size_new_census + 1),  # Applying log transformation
    mean_log_cache_census = mean(log_cache_census, na.rm = TRUE),
    sd_log_cache_census = sd(log_cache_census, na.rm = TRUE),
    standardized_log_cache_census = (log_cache_census - mean_log_cache_census) / sd_log_cache_census  # Standardization
  ) %>%
  ungroup()

#histogram of log transformed + standardized caches
ggplot(cache_intrusions, aes(x = standardized_log_cache_census)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Standardized Log-Transformed Cache Sizes",
       x = "Standardized Log Cache Size", y = "Frequency")


# Do males cache more to compensate for stolen cones? ---------------------
##filter for only male middens being intruded on
cache_intrusions_m <- cache_intrusions %>%
  filter(sex_census == "M")  

#filter for both groups
# Filter and prepare data for both groups
never_intruded <- filter(cache_intrusions_m, all_unintruded == TRUE)$standardized_log_cache_census
been_intruded <- filter(cache_intrusions_m, all_unintruded == FALSE)$standardized_log_cache_census

#run a wilcoxon signed-rank test: data not normally distributed and paired samples
#check for sufficient data points in each group
if(length(never_intruded) > 0 && length(been_intruded) > 0) {
  #conduct the wilcoxon rank-sum test
  test_result <- wilcox.test(never_intruded, been_intruded, alternative = "two.sided")
  print(test_result)
} else {
  cat("One of the groups has no data points. Please check your data.")
}


#perform a cliff's delta to measure effect size (the magnitude of an observed effect, independent of sample size)
cliff_delta <- cliff.delta(never_intruded, been_intruded)
print(cliff_delta)



# plots -------------------------------------------------------------------

#boxplot - highlights means, quartiles and potential outliers, giving a complete picture 
ggplot(cache_intrusions_m, aes(x = factor(all_unintruded), y = standardized_log_cache_census, fill = factor(all_unintruded))) +
  geom_boxplot() +
  labs(title = "Comparison of Cache Sizes in Male Middens",
       x = "Midden Intrusion Status (Never Intruded = TRUE)",
       y = "Standardized Log Cache Size",
       fill = "Intrusion Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", labels = c("Intruded", "Not Intruded"))

#violin plot - useful if the data is not symmetric or if there are multiple modes
ggplot(cache_intrusions_m, aes(x = factor(all_unintruded), y = standardized_log_cache_census, fill = factor(all_unintruded))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Cache Size Distributions by Intrusion Status",
       x = "Midden Intrusion Status",
       y = "Standardized Log Cache Size",
       fill = "Intrusion Status") +
  theme_minimal()

#density plot - useful if data is skewed or contains outliers
ggplot(cache_intrusions_m, aes(x = standardized_log_cache_census, fill = factor(all_unintruded))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Cache Sizes Across Intrusion Status",
       x = "Standardized Log Cache Size",
       y = "Density",
       fill = "Intrusion Status")








