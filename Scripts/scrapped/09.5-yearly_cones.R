#load packages
source("Scripts/00-packages.R")

#connection to KRSP databases
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

tree_cones <-tbl (con, "cones") %>%
  filter(Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         cone_index = log(NumNew + 1),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)) # according to Krebs et al. 2012
  )

#mean cones per year
yearly_cones <- group_by(tree_cones, Year, Grid) %>% 
  dplyr::summarize(num_trees = sum(!is.na(NumNew)),
                   cone_counts = mean(NumNew, na.rm = TRUE),
                   cone_index = mean(cone_index, na.rm = TRUE),
                   total_cones = mean(total_cones, na.rm = TRUE))

#log
yearly_cones$log_total_cones <- log(yearly_cones$total_cones + 1)

#save
write.csv(yearly_cones, "Input/yearly_cones.csv", row.names = FALSE)







