library(tidyverse)
library(scales)
library(RColorBrewer)

#Set your working directory- this is problematic for me because it won't let me change my wd
setwd("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/")
# folders for each seed
list_folders <- list.files(path =  "C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/")
list_folders <- list_folders[4:9]

data <- data.frame()
data <- cbind(read.csv(paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/SEED1/", list_files[i]))$update)
for (i in 1:length(list_folders)){
  list_files <- list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[i]))
  for (j in 1:length(list_files)){
    data <- cbind(data, read.csv(paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[i], "/", list_files[j]))$mean_p53)
    # rename(filename, str_glue(str_glue("file_", list_files[i])))  
  } 
}
list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[1]))
# get all column names
col_names <- c(list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[1])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[2])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[3])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[4])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[5])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[6])))


data <- as.tibble(data)
colnames(data)[1] <- "Update"
colnames(data)[2:1051] <- as.character(col_names)

data_trim <- data %>%
  select(colnames(data)[1], matches("benig0.5") & matches("malig0.5")) %>%
  pivot_longer(
    cols = (SP1malig0.5benig0.5p530.0.dat:SP6malig0.5benig0.5p530.2.dat),
    names_to = "starting_P53",
    values_to = "Mean_p53") %>%
  separate(starting_P53,
           c("Seed", "Starting_P53"),
           sep = 3) %>%
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_P53 = as.factor(Starting_P53)) %>%
  mutate(Starting_P53 = fct_recode(Starting_P53,
             "0.0 P53" = "malig0.5benig0.5p530.0.dat",
             "0.05 P53" = "malig0.5benig0.5p530.05.dat",
             "0.06 P53" = "malig0.5benig0.5p530.06.dat",
             "0.08 P53" = "malig0.5benig0.5p530.08.dat",
             "0.13 P53" = "malig0.5benig0.5p530.13.dat",
             "0.15 P53" = "malig0.5benig0.5p530.15.dat",
             "0.2 P53" = "malig0.5benig0.5p530.2.dat"))
ggplot(data_trim, aes(x = Update, y = Mean_p53, color = Starting_P53)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed))