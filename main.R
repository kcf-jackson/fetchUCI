rm(list = ls())
library(curl)
library(rvest)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)

# Organise UCI datasets in a tibble --------------------------------------------
get_hrefs <- function(url, css, dirname = TRUE) {
  pdir <- ifelse(dirname, dirname(url), url)
  
  atags <- url %>% read_html %>% html_nodes(css)
  text <- atags %>% html_text()
  hrefs <- atags %>% html_attr("href") %>% file.path(pdir, .)
  
  return(tibble(text = text, hrefs = hrefs))
}

# dataset landing page
uci_url <- "https://archive.ics.uci.edu/ml/datasets.html"
uci_datasets <- get_hrefs(uci_url, "table p b a ") %>%
  rename(name = text, url = hrefs)

# dataset folder and description
uci_datasets %<>% mutate(
  download_hrefs = map(url, get_hrefs, "table tr td table tr td p a"))
uci_datasets %<>% unnest() %>% 
  spread(text, hrefs)

# files
safe_get_hrefs <- safely(get_hrefs)
uci_datasets %<>% mutate(
  files = map(`Data Folder`, safe_get_hrefs, "body table tr td a",
              dirname = FALSE))

saveRDS(uci_datasets, "uci_datasets.RDS")


# Basic usage ------------------------------------------------------------------
rm(list = ls())
uci_datasets <- readRDS("uci_datasets.RDS")

# Function to fetch description
fetch_description <- function(uci_dataset_name, dest = ".") {
  desc_url <- uci_datasets %>% filter(name == uci_dataset_name) %>% 
    select(`Data Set Description`) %>% as_vector
  
  if(desc_url != "https://archive.ics.uci.edu/ml/datasets/#") {
    dir.create(uci_dataset_name)
    
    desc_file <- file.path(dest, uci_dataset_name, basename(desc_url))
    desc_url %>% curl_download(desc_file)

    cat(readLines(desc_file), sep = "\n")
  }
}

# Function to fetch data
fetch_data <- function(uci_dataset_name, dest = ".") {
  safe_res <- uci_datasets %>% filter(name == uci_dataset_name) %>%
    select(files) %>% extract2(1) %>% extract2(1)
  
  if(is.null(safe_res$error)) {
    dir.create(uci_dataset_name)
    
    file_urls <- safe_res$result %>%
      filter(!text %in% c("Parent Directory", "Index")) %>%
      select(hrefs) %>% unlist
    filenames <- map(file_urls, basename) %>% 
      file.path(dest, uci_dataset_name, .)
    
    res <- map2(file_urls, filenames, curl_fetch_disk)
    res
  }
}

fetch_description("Abalone")
fetch_data("Abalone")
