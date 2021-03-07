library(tidyverse)
library(scales)

library(RColorBrewer)

#Set your working directory- this is problematic for me because it won't let me change my wd
setwd("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/")
# folders for each seed
list_folders <- list.files(path =  "C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/")
list_folders <- list_folders[4:23]
list_folders
# data for all files in all seeds
data <- data.frame()
temp_data_file <-read.csv("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/SEED1/SP1malig0.0benig0.0p530.05.dat")
temp_data_file$update
data <- cbind(temp_data_file$update)
for (i in 1:length(list_folders)){
  list_files <- list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[i]))
  for (j in 1:length(list_files)){
    data <- cbind(data, read.csv(paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[i], "/", list_files[j]))$mean_p53)
  } 
}
# get all column names
col_names <- c(list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[1])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[2])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[3])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[4])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[5])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[6])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[7])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[8])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[9])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[10])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[11])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[12])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[13])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[14])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[15])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[16])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[17])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[18])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[19])),
                    list.files(path = paste0("C:/Users/koell/OneDrive - Carleton College/R/STAT_220/Assignments/empirical-p53-simulator/trial1/", list_folders[20])))
# change column names
col_names
length(col_names)
data <- as_tibble(data)
colnames(data)[1] <- "Update"
colnames(data)[2:3501] <- as.character(col_names)
# trim data set to particular needs
####################################################################
# p53 expression analysis
## we are setting benig constant to 0.5 and malig to 0.5.
malig_0.5_benig_0.5 <- data %>%
  select(colnames(data)[1], matches("benig0.5") & matches("malig0.5")) %>%
  pivot_longer(
    cols = (SP1malig0.5benig0.5p530.0.dat:SP9malig0.5benig0.5p530.2.dat),
    names_to = "starting_P53",
    values_to = "Mean_p53") %>%
  separate(starting_P53, c("Seed", "Starting_P53"),
   sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                          "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                          "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                          "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                          "SP19", "SP20")) %>%
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
  ## plot data
ggplot(malig_0.5_benig_0.5, aes(x = Update, y = Mean_p53, color = Starting_P53)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Malignant 0.5 Benign 0.5") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

## we are setting benig constant to 0.75 and malig to 0.25.
malig_0.25_benig_0.75 <- data %>%
  select(colnames(data)[1], matches("benig0.75") & matches("malig0.25")) %>%
  pivot_longer(
    cols = (SP1malig0.25benig0.75p530.0.dat:SP9malig0.25benig0.75p530.2.dat),
    names_to = "starting_P53",
    values_to = "Mean_p53") %>%
  separate(starting_P53, c("Seed", "Starting_P53"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%  
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_P53 = as.factor(Starting_P53)) %>%
  mutate(Starting_P53 = fct_recode(Starting_P53,
                                   "0.0 P53" = "malig0.25benig0.75p530.0.dat",
                                   "0.05 P53" = "malig0.25benig0.75p530.05.dat",
                                   "0.06 P53" = "malig0.25benig0.75p530.06.dat",
                                   "0.08 P53" = "malig0.25benig0.75p530.08.dat",
                                   "0.13 P53" = "malig0.25benig0.75p530.13.dat",
                                   "0.15 P53" = "malig0.25benig0.75p530.15.dat",
                                   "0.2 P53" = "malig0.25benig0.75p530.2.dat"))
## plot data
ggplot(malig_0.25_benig_0.75, aes(x = Update, y = Mean_p53, color = Starting_P53)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) +
  ggtitle("Malignant 0.25 Benign 0.75") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


## we are setting benig constant to 0.25 and malig to 0.75.
malig_0.75_benig_0.25 <- data %>%
  select(colnames(data)[1], matches("benig0.25") & matches("malig0.75")) %>%
  pivot_longer(
    cols = (SP1malig0.75benig0.25p530.0.dat:SP9malig0.75benig0.25p530.2.dat),
    names_to = "starting_P53",
    values_to = "Mean_p53") %>%
  separate(starting_P53,
           c("Seed", "Starting_P53"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_P53 = as.factor(Starting_P53)) %>%
  mutate(Starting_P53 = fct_recode(Starting_P53,
                                   "0.0 P53" = "malig0.75benig0.25p530.0.dat",
                                   "0.05 P53" = "malig0.75benig0.25p530.05.dat",
                                   "0.06 P53" = "malig0.75benig0.25p530.06.dat",
                                   "0.08 P53" = "malig0.75benig0.25p530.08.dat",
                                   "0.13 P53" = "malig0.75benig0.25p530.13.dat",
                                   "0.15 P53" = "malig0.75benig0.25p530.15.dat",
                                   "0.2 P53" = "malig0.75benig0.25p530.2.dat"))
## plot data
ggplot(malig_0.75_benig_0.25, aes(x = Update, y = Mean_p53, color = Starting_P53)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) +
  ggtitle("Malignant 0.75 Benign 0.25") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

###################################################################
# malignant mutation rate analysis
## we are setting benig constant to 0.5 and P53 to 0.08.
benig_0.5_p530.08 <- data %>%
  select(colnames(data)[1], matches("p530.08") & matches("benig0.5")) %>% 
  pivot_longer(
    cols = (SP1malig0.0benig0.5p530.08.dat:SP9malig0.9benig0.5p530.08.dat),
    names_to = "starting_malig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_malig_rate,
           c("Seed", "Starting_Malig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%      
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Malig_Rate = as.factor(Starting_Malig_Rate)) %>%
  mutate(Starting_Malig_Rate = fct_recode(Starting_Malig_Rate,
                                   "0.0 Malig Rate" = "malig0.0benig0.5p530.08.dat",
                                   "0.25 Malig Rate" = "malig0.25benig0.5p530.08.dat",
                                   "0.5 Malig Rate" = "malig0.5benig0.5p530.08.dat",
                                   "0.75 Malig Rate" = "malig0.75benig0.5p530.08.dat",
                                   "0.9 Malig Rate" = "malig0.9benig0.5p530.08.dat"))
## plot data
ggplot(benig_0.5_p530.08, aes(x = Update, y = Mean_p53, color = Starting_Malig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Benign 0.5 P53 0.08") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

## we are setting benig constant to 0.25 and P53 to 0.15.
benig_0.25_p530.15 <- data %>%
  select(colnames(data)[1], matches("p530.15") & matches("benig0.25")) %>% 
  pivot_longer(
    cols = (SP1malig0.0benig0.25p530.15.dat:SP9malig0.9benig0.25p530.15.dat),
    names_to = "starting_malig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_malig_rate,
           c("Seed", "Starting_Malig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Malig_Rate = as.factor(Starting_Malig_Rate)) %>%
  mutate(Starting_Malig_Rate = fct_recode(Starting_Malig_Rate,
                                          "0.0 Malig Rate" = "malig0.0benig0.25p530.15.dat",
                                          "0.25 Malig Rate" = "malig0.25benig0.25p530.15.dat",
                                          "0.5 Malig Rate" = "malig0.5benig0.25p530.15.dat",
                                          "0.75 Malig Rate" = "malig0.75benig0.25p530.15.dat",
                                          "0.9 Malig Rate" = "malig0.9benig0.25p530.15.dat"))
## plot data
ggplot(benig_0.25_p530.15, aes(x = Update, y = Mean_p53, color = Starting_Malig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Benign 0.25 P53 0.15") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


## we are setting benig constant to 0.75 and P53 to 0.05.
benig_0.75_p530.05 <- data %>%
  select(colnames(data)[1], matches("p530.05") & matches("benig0.75")) %>% 
  pivot_longer(
    cols = (SP1malig0.0benig0.75p530.05.dat:SP9malig0.9benig0.75p530.05.dat),
    names_to = "starting_malig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_malig_rate,
           c("Seed", "Starting_Malig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Malig_Rate = as.factor(Starting_Malig_Rate)) %>%
  mutate(Starting_Malig_Rate = fct_recode(Starting_Malig_Rate,
                                          "0.0 Malig Rate" = "malig0.0benig0.75p530.05.dat",
                                          "0.25 Malig Rate" = "malig0.25benig0.75p530.05.dat",
                                          "0.5 Malig Rate" = "malig0.5benig0.75p530.05.dat",
                                          "0.75 Malig Rate" = "malig0.75benig0.75p530.05.dat",
                                          "0.9 Malig Rate" = "malig0.9benig0.75p530.05.dat"))
## plot data
ggplot(benig_0.75_p530.05, aes(x = Update, y = Mean_p53, color = Starting_Malig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Benign 0.75 P53 0.05") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))
###################################################################
# benign mutation rate analysis
## we are setting malig constant to 0.5 and P53 to 0.08.
malig_0.5_p530.08 <- data %>%
  select(colnames(data)[1], matches("p530.08") & matches("malig0.5")) %>% 
  pivot_longer(
    cols = (SP1malig0.5benig0.0p530.08.dat:SP9malig0.5benig0.9p530.08.dat),
    names_to = "starting_benig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_benig_rate,
           c("Seed", "Starting_Benig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Benig_Rate = as.factor(Starting_Benig_Rate)) %>%
  mutate(Starting_Benig_Rate = fct_recode(Starting_Benig_Rate,
                                          "0.0 Benig Rate" = "malig0.5benig0.0p530.08.dat",
                                          "0.25 Benig Rate" = "malig0.5benig0.25p530.08.dat",
                                          "0.5 Benig Rate" = "malig0.5benig0.5p530.08.dat",
                                          "0.75 Benig Rate" = "malig0.5benig0.75p530.08.dat",
                                          "0.9 Benig Rate" = "malig0.5benig0.9p530.08.dat"))
## plot data
ggplot(malig_0.5_p530.08, aes(x = Update, y = Mean_p53, color = Starting_Benig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Malig 0.5 P53 0.08") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

## we are setting benig constant to 0.25 and P53 to 0.15.
malig_0.25_p530.15 <- data %>%
  select(colnames(data)[1], matches("p530.15") & matches("malig0.25")) %>% 
  pivot_longer(
    cols = (SP1malig0.25benig0.0p530.15.dat:SP9malig0.25benig0.9p530.15.dat),
    names_to = "starting_benig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_benig_rate,
           c("Seed", "Starting_Benig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Benig_Rate = as.factor(Starting_Benig_Rate)) %>%
  mutate(Starting_Benig_Rate = fct_recode(Starting_Benig_Rate,
                                          "0.0 Benig Rate" = "malig0.25benig0.0p530.15.dat",
                                          "0.25 Benig Rate" = "malig0.25benig0.25p530.15.dat",
                                          "0.5 Benig Rate" = "malig0.25benig0.5p530.15.dat",
                                          "0.75 Benig Rate" = "malig0.25benig0.75p530.15.dat",
                                          "0.9 Benig Rate" = "malig0.25benig0.9p530.15.dat"))
## plot data
ggplot(malig_0.25_p530.15, aes(x = Update, y = Mean_p53, color = Starting_Benig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Malig 0.25 P53 0.15") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


## we are setting malig constant to 0.75 and P53 to 0.05.
malig_0.75_p530.05 <- data %>%
  select(colnames(data)[1], matches("p530.05") & matches("malig0.75")) %>% 
  pivot_longer(
    cols = (SP1malig0.75benig0.0p530.05.dat:SP9malig0.75benig0.9p530.05.dat),
    names_to = "starting_benig_rate",
    values_to = "Mean_p53") %>%
  separate(starting_benig_rate,
           c("Seed", "Starting_Benig_Rate"),
           sep = "(?<=.)(?=[m])") %>%
  mutate(Seed = as.factor(Seed)) %>%
  mutate(Seed = fct_relevel(Seed, 
                            "SP1", "SP2", "SP3", "SP4", "SP5", "SP6",
                            "SP7", "SP8", "SP9", "SP10", "SP11", "SP12",
                            "SP13", "SP14", "SP15", "SP16", "SP17", "SP18", 
                            "SP19", "SP20")) %>%    
  filter(Mean_p53 > 0.05 & Mean_p53 <0.15) %>%
  mutate(Starting_Benig_Rate = as.factor(Starting_Benig_Rate)) %>%
  mutate(Starting_Benig_Rate = fct_recode(Starting_Benig_Rate,
                                          "0.0 Benig Rate" = "malig0.75benig0.0p530.05.dat",
                                          "0.25 Benig Rate" = "malig0.75benig0.25p530.05.dat",
                                          "0.5 Benig Rate" = "malig0.75benig0.5p530.05.dat",
                                          "0.75 Benig Rate" = "malig0.75benig0.75p530.05.dat",
                                          "0.9 Benig Rate" = "malig0.75benig0.9p530.05.dat"))
## plot data
ggplot(malig_0.75_p530.05, aes(x = Update, y = Mean_p53, color = Starting_Benig_Rate)) + 
  geom_jitter() + 
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(vars(Seed)) + 
  ggtitle("Malig 0.75 P53 0.05") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))