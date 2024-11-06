devtools::install_github("KluaneRedSquirrelProject/krsp")
library(krsp)
library(data.table)
library(ggplot2)

# Connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# Connection to krsp_suppl database
con3 <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                      dbname ="krsp_suppl",
                      username = Sys.getenv("krsp_user"),
                      password = Sys.getenv("krsp_password")
)

# Import data -------------------------------------------------------------

# The supplementary tables are not updated in the annual data cleanup, so squirrel_id values must be updated from the historic_squirrel_ids table
historic_ids<- tbl(con, "historic_squirrel_ids") %>% 
  select(old_squirrel_id, new_squirrel_id) %>% 
  collect()

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL", "CH", "AG", "LL", "JO", "RR", "SX")) %>% 
  select(squirrel_id, sex, byear) %>% 
  collect()

# Midden cone data

midden_cones<-tbl(con3, "midden_cones") %>% 
  filter(squirrel_id !="UTS") %>% #remove UTS squirrels from data
  collect() %>% 
  left_join (historic_ids, by=c("squirrel_id" = "old_squirrel_id")) %>% 
  mutate (squirrel_id = ifelse(is.na(new_squirrel_id), squirrel_id, new_squirrel_id),
          date = as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S')) # date looks to be in datetime format

# Filter for 2023 data

dt <- as.data.table(midden_cones)
class(dt)
dt23 <- dt[year == 2023 & grid == "KL"]

# Set number of significant digits
options(scipen = 999)
options(digits = 6)

dt23[, midden_area := (pi * (length/2) * (width/2))]
dt23[, midden_cones := ((total_cones/no_quad) * (midden_area/area_quad))]

fwrite(dt23, "Output/dt23.csv")

ggplot(dt23[sex == "M" | sex == "F"]) + 
  geom_histogram(aes(x = midden_cones)) +
  facet_wrap(~ sex)

M23 <- dt23[sex == "M"]
F23 <- dt23[sex == "F"]

fwrite(M23, "Output/M23.csv")
fwrite(F23, "Output/F23.csv")

# Bin midden cones

# Males -------------------------------------------------------------------

Mquantiles <- quantile(M23$midden_cones, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# Define bin breaks based on quartiles
Mlow_break <- Mquantiles[2]
Mmedium_break <- Mquantiles[3]

M23[, cone_category := cut(midden_cones, breaks = c(-Inf, Mlow_break, Mmedium_break, Inf), 
                            labels = c("Low cones", "Medium cones", "High cones"))]

M23 <- M23[!grepl("-8|-7|-6|O|P|15|16|17|18|19", M23$midden), ]

M23[, .N, by = .(cone_category)]

ggplot(M23, aes(x = cone_category)) +
  geom_bar() +
  labs(x = "Cone Category", y = "Number of Individuals")

M23 <- M23[, .(midden, sex, taglft, tagrt, cone_category)]


# Females -----------------------------------------------------------------

Fquantiles <- quantile(F23$midden_cones, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# Define bin breaks based on quartiles
Flow_break <- Fquantiles[2]
Fmedium_break <- Fquantiles[3]

F23[, cone_category := cut(midden_cones, 
                           breaks = c(-Inf, Flow_break, Fmedium_break, Inf), 
                           labels = c("Low cones", "Medium cones", "High cones"))]

F23 <- F23[!grepl("-8|-7|-6|O|P|15|16|17|18|19", F23$midden), ]

F23 <- F23[!is.na(cone_category)]

F23[, .N, by = .(cone_category)]

ggplot(F23, aes(x = cone_category)) +
  geom_bar() +
  labs(x = "Cone Category", y = "Number of Individuals")

F23 <- F23[, .(midden, sex, taglft, tagrt, cone_category)]

# Select middens

# Males -------------------------------------------------------------------
  
# Randomly sample 7 middens for "High cones" category
Mhigh_cones <- M23[cone_category == "High cones", sample(midden, 7)]

# Randomly sample 7 middens for "Medium cones" category
Mmedium_cones <- M23[cone_category == "Medium cones", sample(midden, 8)]

print(Mmedium_cones)

midsM23 <- M23[cone_category == "High cones" | cone_category == "Medium cones",
               .(midden = sample(midden, 8)), by = cone_category]

KLCameras_M <- M23[midden %in% midsM23$midden]

fwrite(KLCameras_M, "Output/KLCameras_M.csv")

# Females -----------------------------------------------------------------

midsF23 <- F23[cone_category == "High cones", 
               .(midden = sample(midden, 5)), by = cone_category]

KLCameras_F <- F23[midden %in% midsF23$midden]

fwrite(KLCameras_F, "Output/KLCameras_F.csv")






