#------------------------------------------------------------------------------#
# DATA WRANGLING
#------------------------------------------------------------------------------#
here::i_am("scripts/data_wrangling.R")
library(tidyverse)
library(here)
source(here("scripts/plot_style.R"))

# File paths
data_file_path <- here("data/2025_aDNAArch_data.csv")
affi_file_path <- here("data/2025_aDNAArch_affiliation.csv")
auth_file_path <- here("data/2025_aDNAArch_author_affiliation.csv")

#------------------------------------------------------------------------------#
# Load dataset ####
#------------------------------------------------------------------------------#
# Load data
data <- readr::read_csv(file = data_file_path, col_select = -c(22,'notes')) %>%
  mutate(CitationID = sub("a$", "", CitationID)) %>%
  rename_with(~gsub("[ ]","_",.)) %>%
  rename(C14 = "14C") %>%
  arrange(CitationID,aDNAID)

# Sanity check for missing data (all fields should be filled)
filter(data, if_any(everything(), is.na))

# Extract samples' metadata
samp_metadata_new <- data %>%
  filter(Resampled == 0) %>%
  group_by(CitationID) %>%
  summarise(n_samples = n(),
            n_countries = n_distinct(Country)) %>%
  mutate(dataset = "new")

samp_metadata_all <- data %>%
  group_by(CitationID) %>%
  summarise(n_samples = n(),
            n_countries = n_distinct(Country)) %>%
  mutate(dataset = "all")

samp_metadata <- rbind(samp_metadata_new,samp_metadata_all) %>% arrange(CitationID)

#------------------------------------------------------------------------------#
# Transform data into presence/absence and proportions ####
#------------------------------------------------------------------------------#
data <- data %>%
  mutate_at(vars(-(c(aDNAID, Resampled, Country, CitationID))), ~ ifelse(. == "not applicable", NA, .)) %>%
  mutate_at(vars(-(c(aDNAID, Resampled, Country, CitationID))), as.numeric) %>%
  mutate_at(vars(-(c(aDNAID, Resampled, Country, CitationID))), ~ ifelse(. > 0 & !is.na(.), 1, .)) %>%
  mutate_at(vars(-(c(aDNAID, Resampled, Country, CitationID))), ~ ifelse(is.na(.), 0.5, .))
# 1: reported / 0: not reported / 0.5: not reported but previously published

# Aggregate presence/absence per sample into proportion per publication
data_new <- filter(data, Resampled == 0)
# "_new": incl. only newly sequenced samples
# There are only very few resequenced samples so it did not make much of a difference.
# We decided to go with the full dataset

data_tmp <- select(data, -c(aDNAID, Country, Resampled))
data_new_tmp <- select(data_new, -c(aDNAID, Country, Resampled))

data_p <- data_tmp %>% group_by(CitationID) %>% summarise_all(mean) %>% ungroup()
data_c <- data_tmp %>% group_by(CitationID) %>% summarise_all(sum) %>% ungroup()
data_c_pre <- data_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 1)) %>% ungroup()
data_c_na <- data_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 0.5)) %>% ungroup()
data_c_abs <- data_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 0)) %>% ungroup()

# data_new_p <- data_new_tmp %>% group_by(CitationID) %>% summarise_all(mean) %>% ungroup()
# data_new_c <- data_new_tmp %>% group_by(CitationID) %>% summarise_all(sum) %>% ungroup()
# data_new_c_pre <- data_new_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 1)) %>% ungroup()
# data_new_c_na <- data_new_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 0.5)) %>% ungroup()
# data_new_c_abs <- data_new_tmp %>% group_by(CitationID) %>% summarise_all(~sum(. == 0)) %>% ungroup()
# p: proportion / c: count / pre: presence / abs: absence

#------------------------------------------------------------------------------#
# Author and affiliation metadata ####
#------------------------------------------------------------------------------#
# Load data
affi <- readr::read_csv(file = affi_file_path) # affiliation's Archaeology Index
auth <- readr::read_csv(file = auth_file_path) # author's affiliations

# Extract author's metadata
auth_metadata <- auth %>% 
  mutate(CitationID = sub("a$", "", CitationID)) %>%
  left_join(affi, by = join_by(Affiliation)) %>%
  rename(arch_index_affi = "Archaeology Index") %>%
  group_by(CitationID, Name) %>%
  mutate(arch_index_ind = mean(arch_index_affi)) %>%
  group_by(CitationID) %>%
  summarise(n_authors = n_distinct(Name),
            arch_index = mean(arch_index_ind)) %>%
  ungroup()

#------------------------------------------------------------------------------#
# Publication metadata ####
#------------------------------------------------------------------------------#
# Extract last name of 1st author and year of publication from CitationID
CitationID_parsed <- as_tibble(
  do.call(
    rbind,
    str_split(
      auth_metadata$CitationID,
      "(?<=[0-9])(?=[A-Za-z])|(?<=[A-Za-z])(?=[0-9])"
    )
  )
) %>%
  rename(first_author = V1,
         year_publication = V2)

publ_metadata <- CitationID_parsed %>%
  bind_cols(CitationID = auth_metadata$CitationID) %>%
  mutate(year_publication = as.numeric(year_publication)) %>%
  arrange(year_publication, first_author)

# Order of publications
CitationID_order <- unique(publ_metadata$CitationID)

#------------------------------------------------------------------------------#
# All metadata ####
#------------------------------------------------------------------------------#
metadata <- samp_metadata %>%
  full_join(auth_metadata, by = join_by(CitationID)) %>%
  full_join(publ_metadata, by = join_by(CitationID)) %>%
  mutate(n_authors_log = log(n_authors),
         n_countries_log = log(n_countries),
         n_samples_log = log(n_samples)) %>%
  arrange(year_publication,first_author)

#------------------------------------------------------------------------------#
# Objects for plotting ####
#------------------------------------------------------------------------------#
# Data broadly fall into two categories - core and extended
extended_cols <- c("Anthropological_age", "Anthropological_sex",
                   "Body_arrangement", "Body_positioning", "C14",
                   "Culture", "Grave", "Grave_goods", "Images")

# Long format of data for plotting
data_p_long <- data_p %>%
  pivot_longer(names_to = "data", values_to = "proportion", cols = 2:17) %>%
  mutate(CitationID = factor(CitationID, levels = CitationID_order), # relevel CitationID
         category = ifelse(data %in% extended_cols, # add categories
                           "Extended", "Core"),
         category = factor(category, levels = c("Core", "Extended"))) %>%
  arrange(CitationID,category,data)

data_c_long <- data_c_pre %>%
  pivot_longer(names_to = "data", values_to = "presence", cols = 2:17) %>%
  left_join(data_c_na %>%
              pivot_longer(names_to = "data", values_to = "NA", cols = 2:17)) %>%
  left_join(data_c_abs %>%
              pivot_longer(names_to = "data", values_to = "absence", cols = 2:17))  %>%
  pivot_longer(names_to = "presence_absence", values_to = "count", cols = c(presence,absence,`NA`)) %>%
  mutate(CitationID = factor(CitationID, levels = CitationID_order), # relevel CitationID
         category = ifelse(data %in% extended_cols, # add categories
                           "Extended", "Core"),
         category = factor(category, levels = c("Core", "Extended"))) %>%
  arrange(CitationID,category,data)

# Order of metadata fields
y_lab_order <- unique(data_p_long$data)

#------------------------------------------------------------------------------#
# Objects for data analyses ####
#------------------------------------------------------------------------------#
y <- data_p %>%
  mutate(CitationID = factor(CitationID, levels = CitationID_order)) %>%
  arrange(CitationID) %>% # order of rows is important
  column_to_rownames(var = "CitationID") %>%
  select(all_of(y_lab_order)) # reorder columns

x <- metadata %>%
  filter(dataset == "all") %>%
  select(c(CitationID, n_samples_log, n_countries_log,
           n_authors_log, arch_index, year_publication)) %>%
  column_to_rownames(var = "CitationID") %>%
  select(order(colnames(.)))

x_ori <- metadata %>%
  filter(dataset == "all") %>%
  select(c(CitationID, n_samples, n_countries,
           n_authors, arch_index, year_publication)) %>%
  column_to_rownames(var = "CitationID") %>%
  select(order(colnames(.)))

pca_y <- FactoMineR::PCA(y, scale.unit = TRUE, graph = FALSE, ncp = 16)
rownames(pca_y$var$coord) <- lab_mapping[rownames(pca_y$var$coord)]

pca_x <- FactoMineR::PCA(x, scale.unit = TRUE, graph = FALSE, ncp = 5) 
rownames(pca_x$var$coord) <- lab_mapping[rownames(pca_x$var$coord)]

#------------------------------------------------------------------------------#
# Overview of data ####
#------------------------------------------------------------------------------#
# Sample scope in this study
n_samples <- subset(metadata, dataset == "all")$n_samples
summary(n_samples)
n_countries <- subset(metadata, dataset == "all")$n_countries
summary(n_countries)
length(unique(data$Country))

# Percentage of NAs across all observations
all_obs <- as.vector(as.matrix(data[,-c(1:4)]))
sum(all_obs == 0.5)/length(all_obs)*100

