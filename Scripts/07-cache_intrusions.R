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


# tally intrusions by male and by year ----------------------------------------------------------
intruders <- read.csv("Output/intruders.csv")

#create a new data table
cache_intrusions <- intruders %>%
  group_by(squirrel_id_census, year, sex_census) %>%  #grouping by grid, squirrel ID, and year
  summarise(intrusion_count = sum(intruder, na.rm = TRUE), .groups = 'drop') %>%  #summing the intrusion counts
  rename(squirrel_id = squirrel_id_census,
         sex = sex_census)


# add in new cones cached -------------------------------------------------
cache_intrusions <- left_join(cache_intrusions, midden_cones %>% 
                    dplyr::select(squirrel_id, year, cache_size_new), 
                           by = c("squirrel_id", "year")) %>%
                    na.omit()


# add in cone crop --------------------------------------------------------
tree_cones <-tbl (con_suppl, "cones") %>%
  filter(Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         cone_index = log(NumNew + 1),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)) # according to Krebs et al. 2012
  )

#mean cones per year
yearly_cones <- group_by(tree_cones, Year) %>% 
  summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

#must add in data for 1989 because there were no cones but the zeros were not entered
yearly_cones <- rbind(yearly_cones,
                      list(1989L, 0, 0, 0, 0.005525156)) %>%
                rename(year = Year)


# merge everything together -----------------------------------------------
cache_intrusions <- cache_intrusions %>%
  left_join(yearly_cones %>% dplyr::select(year, total_cones), by = "year") %>%
  na.omit()

#sex needs to be a factor
cache_intrusions$sex <- as.factor(cache_intrusions$sex)

#reorder columns
cache_intrusions <- cache_intrusions %>%
  dplyr::select(year, squirrel_id, sex, intrusion_count, cache_size_new, total_cones)

# investigate the data ----------------------------------------------------
summary(cache_intrusions)
str(cache_intrusions)

#histograms for continuous data
hist(cache_intrusions$cache_size_new, main="Histogram of New Cone Caches", xlab="New Cone Caches")
hist(cache_intrusions$total_cones, main="Histogram of Total Cones", xlab="Total Cones")

#bar graph for categorical data
table(cache_intrusions$sex)
barplot(table(cache_intrusions$sex), main="Distribution of Sex", ylab="Frequency", col="blue")

#compare cache_size_new between sexes
boxplot(cache_size_new ~ sex, data = cache_intrusions, main="Cache Size by Sex", ylab="New Cone Caches", col=c("pink", "cyan"))

#scatter plot between 
plot(cache_size_new ~ total_cones, data = cache_intrusions, main="Cache Size vs. Total Cones", xlab="Total Cones", ylab="New Cone Caches", pch=19, col=cache_intrusions$sex)
legend("topright", legend=c("Male", "Female"), col=c("pink", "cyan"), pch=19)

#data distributions
par(mfrow=c(1,2))  #setting up the plotting area to display two plots side by side
hist(cache_intrusions$cache_size_new, main="New Cones Cached", xlab="New Cones Cached")
hist(cache_intrusions$intrusion_count, main="Intrusion Count", xlab="Intrusion Count")


ggplot(cache_intrusions, aes(x = intrusion_count)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Intrusion Count", x = "Intrusion Count", y = "Frequency")

ggplot(cache_intrusions, aes(x = cache_size_new)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of New Cones Cached", x = "New Cones Cached", y = "Frequency")

#summary stats
summary_stats <- summary(cache_intrusions[, c("intrusion_count", "cache_size_new")])
print(summary_stats)

#scatter plots
ggplot(cache_intrusions, aes(x=intrusion_count, y=cache_size_new)) +
  geom_point(aes(color=sex), alpha=0.6) +
  facet_wrap(~sex) +
  labs(title="Cache Size vs. Intrusion Count, Faceted by Sex", x="Intrusion Count", y="New Cone Caches") +
  theme_minimal()


# data transformation - since non-normal distribution ---------------------
##log transformations - +1 because data contains zeros and cannot do log(0)
cache_intrusions$log_cache_size_new <- log(cache_intrusions$cache_size_new + 1)
cache_intrusions$log_total_cones <- log(cache_intrusions$total_cones + 1)

#visualizing the transformed data to check for improvements in distribution
par(mfrow=c(1,2))  #setting up the plotting area to display two plots side by side
hist(cache_intrusions$log_cache_size_new, main="Log Transformed Cache Size", xlab="Log of New Cone Caches + 1")

# building the model - WHAT TYPE!?!?!? ---------------------------
##data still non-normally distributed after transformations, therefore need to use a glm with a non-normal distribution
##full_model <- glm(cache_size_new ~ intrusion_count * sex * total_cones, family = Gamma(link = "log"), data = cache_intrusions)
###can't use a glm with gamma because of 0's and values approaching 0 in both cache_size_new and intrusion_count

#fit the linear model
model_linear <- lm(cache_size_new ~ sex * intrusion_count + total_cones, data = cache_intrusions)
summary(model_linear)

#plot residuals to check assumptions
par(mfrow = c(2, 2))
plot(model_linear)

#compare to linear model with log transformation (adding 1 to avoid log(0) issues)
model_log <- lm(log_cache_size_new ~ sex * intrusion_count + log_total_cones, data = cache_intrusions)
summary(model_log)
plot(model_log)

#compare models via AIC, BIC
AIC(model_linear, model_log)
BIC(model_linear, model_log)
#log of cache_size_new & total_cones is better


# Model ------------------------------------------------------------
#create a binary indicator for BOTH log_cache_size_new and intrusion_count
cache_intrusions$cache_present = as.numeric(cache_intrusions$log_cache_size_new > 0)
cache_intrusions$intrusion_present <- as.numeric(cache_intrusions$intrusion_count > 0)

#fit a logistic regression model to predict whether caching and/or intrusions occur
logistic_model <- glm(cache_present ~ sex * intrusion_count * intrusion_present + sex + log_total_cones, family = binomial, data = cache_intrusions)
summary(logistic_model)

##move on to step 2
#subset the data to only include observations where caching occurs
positives <- cache_intrusions %>%
  filter(cache_present == 1)

#model setup
model_positives <- lm(log_cache_size_new ~ sex + sex * intrusion_count * intrusion_present + log_total_cones, data = positives)
summary(model_positives)

#model diagnostics
par(mfrow=c(2,2))
plot(model_positives)

#generate predictions for scatter plots
##create predictions table
predictions <- predict(model_positives, newdata = positives, type = "response")
length(predictions)

##add these predictions to 'positives' dataframe for plotting
positives$predicted_cache <- predictions

#scatter plot
ggplot(positives, aes(x = intrusion_count, y = predicted_cache, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", aes(group = sex), se = FALSE) +  #add a linear regression line for each sex
  labs(x = "Intrusion Count", y = "Predicted Log Cache Size New",
       title = "Model Predictions of Log Cache Size New by Intrusion Count and Sex") +
  scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
  theme_minimal()

#effect plot with jtools
effect_plot(model = model_positives, pred = "intrusion_count", modx = "sex", mod2 = "intrusion_present", data = positives, 
            interval = TRUE, plot.points = TRUE)

















