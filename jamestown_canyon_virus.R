# # Usage
# 
# Required: Docker, dependencies. For context I am running this through my R terminal with git bash. 
# 
# From the command line `nextstrain update` (pull the latest updates)
# 
# # download NCBI data (sequences and metadata)
#I need to update the ingest/defaults/config file taxon id for James canyone virus : 35511
  
# nextstrain build ingest
# <!-- This is a note to self that the parent data folder should be cleared. I understand this note might be visible in the source code -->
#   
# mkdir -p phylogenetic/data  #creating a directory or folder

#   cp -r ingest/results/* phylogenetic/data # this will copy the content
#   nextstrain build phylogenetic

pacman::p_load(rio, here, tidyverse, gtsummary, gt)

jamestown_canyon_metadata <- import("Vector_Borne_Diseases/jamestown_canyon_virus/ingest/results/metadata.tsv") %>% 
  # Remove rows with missing critical data
  filter(!is.na(strain) & !is.na(region) & !is.na(country)) %>% 
  # cleaning some missing and misformated data columns 
  mutate(division = ifelse(is.na(division), "Unknown", division),
         location = ifelse(is.na(location), "Unknown", location), 
         host = ifelse(is.na(host), "Unknown", host),
         # Cleaning the date column 
         date = case_when(
           date == "XXXX-XX-XX" ~ as.character(date_released),
           grepl("^\\d{4}-XX-XX$", date) ~ paste0(substr(date, 1, 4), "-06-30"),
           grepl("^\\d{4}-\\d{2}-XX$", date) ~ paste0(substr(date, 1, 7), "-15"),
           TRUE ~ date ), 
         # date = ifelse(is.na(date), date_released, date), 
         date = ymd(date), 
         year = year(date), 
         date_released = ymd(date_released), 
         date_updated = ymd(date_updated)) 

data <- jamestown_canyon_metadata %>%
  mutate(year = year(date))

# Summarize results by Country and Year
summary_data <- data %>%
  group_by(country, year) %>%
  summarize(
    count = n(),
    unique_strains = n_distinct(strain),
    unique_hosts = n_distinct(host)
  )

# Bar plot of the number of records per country and year
ggplot(summary_data, aes(x = year, y = count, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Records per Country and Year",
       x = "Year",
       y = "Number of Records") +
  theme_minimal()

# Line plot of the number of unique strains per country and year
ggplot(summary_data, aes(x = year, y = unique_strains, color = country)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Unique Strains per Country and Year",
       x = "Year",
       y = "Number of Unique Strains") +
  theme_minimal()

# Line plot of the number of unique hosts per country and year
ggplot(summary_data, aes(x = year, y = unique_hosts, color = country)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Unique Hosts per Country and Year",
       x = "Year",
       y = "Number of Unique Hosts") +
  theme_minimal()




