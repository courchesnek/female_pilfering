#load packages
source("Scripts/00-packages.R")

#read in data
cache_intrusions <- read.csv("Input/cache_intrusions.csv")

#sex needs to be a factor
cache_intrusions$sex <- as.factor(cache_intrusions$sex)

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


# Hurdle Model ------------------------------------------------------------
#create a binary indicator for BOTH log_cache_size_new and intrusion_count
# cache_intrusions$cache_present = as.numeric(cache_intrusions$log_cache_size_new > 0)
# cache_intrusions$intrusion_present <- as.numeric(cache_intrusions$intrusion_count > 0)
# 
# #fit a logistic regression model to predict whether caching and/or intrusions occur
# logistic_model <- glm(cache_present ~ sex * intrusion_count * intrusion_present + log_total_cones, family = binomial, data = cache_intrusions)
# summary(logistic_model)
# 
# ##move on to step 2
# #subset the data to only include observations where caching occurs
# positives <- cache_intrusions %>%
#   filter(cache_present == 1)
# 
# #model setup
# model_positives <- lm(log_cache_size_new ~ sex * intrusion_count * intrusion_present + log_total_cones, data = positives)
# summary(model_positives)
# 
# #model diagnostics
# par(mfrow=c(2,2))
# plot(model_positives)
# 
# #generate predictions for scatter plots
# ##create predictions table
# predictions <- predict(model_positives, newdata = positives, type = "response")
# length(predictions)
# 
# ##add these predictions to 'positives' dataframe for plotting
# positives$predicted_cache <- predictions
# 
# #scatter plot
# ggplot(positives, aes(x = intrusion_count, y = predicted_cache, color = sex)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", aes(group = sex), se = TRUE) +
#   labs(x = "Intrusion Count", y = "Predicted Log Cache Size New",
#        title = "Model Predictions of Log Cache Size New by Intrusion Count and Sex") +
#   scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
#   theme_minimal()
# 
# 
# #effect plot with jtools
# effect_plot(model = model_positives, pred = "intrusion_count", moderator = "sex", 
#             facet.var = "intrusion_present", data = positives, 
#             interval = TRUE, plot.points = TRUE,
#             main = "Effects of Intrusion Count on Log Cache Size by Sex and Intrusion Presence")
# 
# #plot predictions using ggplot instead
# # #create a data frame for predictions
# prediction_data <- expand.grid(
#   sex = levels(positives$sex),
#   intrusion_count = seq(min(positives$intrusion_count, na.rm = TRUE), max(positives$intrusion_count, na.rm = TRUE), length.out = 100),
#   intrusion_present = unique(positives$intrusion_present),
#   log_total_cones = mean(positives$log_total_cones, na.rm = TRUE))
# 
# #Generate predictions with confidence intervals
# prediction_data <- cbind(prediction_data, as.data.frame(predict(model_positives, newdata = prediction_data, 
#                                                                 interval = "confidence", level = 0.95)))
# 
# #rename the columns appropriately
# names(prediction_data)[5:7] <- c("fit", "lwr", "upr")
# 
# #plot
# ggplot(prediction_data, aes(x = intrusion_count, y = fit, color = sex)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr, fill = sex), alpha = 0.2) +  # Add confidence intervals as shaded areas
#   geom_line(size = 1.0) +  # Add lines for the fitted values
#   geom_point(data = positives, aes(y = log_cache_size_new), alpha = 0.4) +  # Overlay actual data points
#   facet_wrap(~ intrusion_present) +  # Facet by 'intrusion_present' to show different plots for each level
#   labs(title = "Predicted Log Cache Size by Intrusion Count, Sex, and Intrusion Presence",
#        x = "Intrusion Count", y = "Predicted Log Cache Size New") +
#   scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
#   scale_fill_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
#   theme_minimal()
# 
# 


# keeping intrusions as a single variable ---------------------------------
#create a binary indicator for BOTH log_cache_size_new and intrusion_count
# cache_intrusions$cache_present = as.numeric(cache_intrusions$log_cache_size_new > 0)
# 
# #fit a logistic regression model to predict whether caching occurs
# model <- glm(cache_present ~ sex * intrusion_count + log_total_cones, family = binomial, data = cache_intrusions)
# 
# #step 1: binary outcome model analysis
# summary(model)
# 
# 
# #move on to step 2: model cache size when caching has actually occurred
# ##subset data to include only instances where caching occurred
# positive_caches <- cache_intrusions %>%
#   filter(cache_present == 1)
# 
# 
# #model
# model_positive_caches <- lm(log_cache_size_new ~ sex * intrusion_count + log_total_cones, data = positive_caches)
# 
# #summary and diagnostics
# summary(model_positive_caches)
# plot(model_positive_caches)
# 
# #generate model predictions
# positive_caches$predicted_cache_size_new <- predict(model_positive_caches, newdata = positive_caches, type = "response")
# 
# #scatterplot
# ggplot(positive_caches, aes(x = intrusion_count, y = predicted_cache_size_new, color = sex)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", aes(group = sex), se = TRUE) +
#   labs(x = "Intrusion Count", y = "Predicted Log Cache Size New",
#        title = "Model Predictions of Log Cache Size New by Intrusion Count and Sex") +
#   scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
#   theme_minimal()
# 
# ##by predicted values
# #create a grid of predictor values
# intrusion_range <- seq(min(positive_caches$intrusion_count), max(positive_caches$intrusion_count), by = 1)
# sex_levels <- unique(positive_caches$sex)
# log_cones_mean <- mean(positive_caches$log_total_cones)
# 
# prediction_grid <- expand.grid(
#   sex = sex_levels,
#   intrusion_count = intrusion_range,
#   log_total_cones = log_cones_mean
# )
# 
# #calculate predictions and confidence intervals
# predictions <- predict(model_positive_caches, newdata = prediction_grid, type = "response", se.fit = TRUE)
# prediction_grid$predicted_log_cache_size_new <- predictions$fit
# prediction_grid$lower_ci <- predictions$fit - 1.96 * predictions$se.fit
# prediction_grid$upper_ci <- predictions$fit + 1.96 * predictions$se.fit
# 
# ggplot() +
#   geom_point(data = positive_caches, aes(x = intrusion_count, y = log_cache_size_new, color = sex), alpha = 0.5) +
#   geom_line(data = prediction_grid, aes(x = intrusion_count, y = predicted_log_cache_size_new, color = sex), size = 0.9) +
#   geom_ribbon(data = prediction_grid, aes(x = intrusion_count, ymin = lower_ci, ymax = upper_ci, fill = sex), alpha = 0.4) +
#   scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
#   scale_fill_manual(values = c("pink", "lightblue"), labels = c("Female", "Male")) +
#   labs(x = "Intrusion Count", y = "Predicted Log Cache Size New",
#        title = "Predicted Cache Size New by Intrusion Count and Sex") +
#   theme_minimal()
# 





#two-part model with continuous distribution -----------------------------
#step 1: binary model for zero vs positive outcomes
cache_intrusions$cache_present = as.numeric(cache_intrusions$log_cache_size_new > 0)

logistic_model <- glm(cache_present ~ sex * intrusion_count + log_total_cones,
                      family = binomial(link = "logit"),
                      data = cache_intrusions)

summary(logistic_model)
plot(logistic_model)

#evaluate the binary model
#predicted probabilities
predicted_prob <- predict(logistic_model, type = "response")

#binarize predictions based on a threshold (usually 0.5)
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

#actual classes
actual_class <- cache_intrusions$cache_present

#confusion Matrix
table(Predicted = predicted_class, Actual = actual_class)

#ROC Curve
roc_curve <- roc(actual_class, predicted_prob)
plot(roc_curve, main="ROC Curve")
auc(roc_curve)

#looks good

#step 2: continuous model for positive outcomes only
#filter for positive caching events
positive_caches <- cache_intrusions %>%
  filter(log_cache_size_new > 0)

#linear model for the log of cache size
model_positives <- lm(log_cache_size_new ~ sex * intrusion_count + log_total_cones,
                      data = positive_caches)

summary(model_positives)


#evaluating continuous model (linear regression)
plot(model_positives)

#generate predictions for plotting - need to generate predictions for both models
##predict the probability that caching occurs
cache_intrusions$prob_cache = predict(logistic_model, type = "response")

#create a subset where caching is likely (prob_cache > 0.5)
positive_caches <- cache_intrusions[cache_intrusions$prob_cache > 0.5, ]

#ensure the linear model is fit on the subset where cache_present is true
positive_caches$predicted_cache_size_new = predict(model_positives, newdata = positive_caches, type = "response")

#add a final predicted value column to the original dataset
cache_intrusions$final_predicted_cache_size = ifelse(cache_intrusions$prob_cache > 0.5,
                                                     predict(model_positives, newdata = subset(cache_intrusions, prob_cache > 0.5)),
                                                     0)

#plot
ggplot(cache_intrusions, aes(x = intrusion_count, y = final_predicted_cache_size, color = sex)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", aes(group = sex), se = TRUE) +  
  labs(x = "Intrusion Count", y = "Predicted Log Cache Size New",
       title = "Predicted Cache Size by Intrusion Count and Sex") +
  scale_color_manual(values = c("red", "blue"), labels = c("Female", "Male")) +
  theme_minimal()

#interaction plot
interaction <- interact_plot(model = model_positives, pred = intrusion_count, modx = sex, data = positive_caches, plot.points = TRUE, interval = TRUE) + 
  scale_color_manual(values = c("hotpink", "blue")) +
  labs(
    x = "Intrusion Count",
    y = "Predicted Log New Cones Cached",
    title = "Interaction of Intrusion Count and Sex on New Cones Cached"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),  
    axis.title.x = element_text(color = "black", size = 12), 
    axis.title.y = element_text(color = "black", size = 12),
  )

interaction

ggsave("Output/interaction.jpeg", plot = interaction, width = 8, height = 6)




