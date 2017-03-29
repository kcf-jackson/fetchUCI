rm(list = ls())
library(curl)
library(rvest)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
source("fetch.R")

# # First time running  ---------------------------------------------------------
# # dataset landing page
# uci_url <- "https://archive.ics.uci.edu/ml/datasets.html"
# uci_datasets <- get_hrefs(uci_url, "table p b a ") %>%
#   rename(name = text, url = hrefs)
# 
# # dataset folder and description
# uci_datasets %<>% mutate(
#   download_hrefs = map(url, get_hrefs, "table tr td table tr td p a"))
# uci_datasets %<>% unnest() %>% 
#   spread(text, hrefs)
# 
# # files
# safe_get_hrefs <- safely(get_hrefs)
# uci_datasets %<>% mutate(
#   files = map(`Data Folder`, safe_get_hrefs, "body table tr td a",
#               dirname = FALSE))
# 
# saveRDS(uci_datasets, "uci_datasets.RDS")

# Basic usage ------------------------------------------------------------------
rm(list = ls())
uci_datasets <- readRDS("uci_datasets.RDS")
fetch_description("Abalone")
fetch_data("Abalone")

