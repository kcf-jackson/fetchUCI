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
uci_datasets <- uci_datasets[1:10, ]

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


# Basic usage ------------------------------------------------------------------
chosen <- uci_datasets$name[[1]]

# Download data set description, then cat.
description_url <- uci_datasets %>% filter(name == chosen) %>% 
    select(`Data Set Description`) %>% as_vector
if(description_url != "https://archive.ics.uci.edu/ml/datasets/#") {
    description_url %>% curl_download(basename(.))
    cat(readLines(basename(description_url)), sep = "\n")
}

# Download data files
safe_res <- uci_datasets %>% filter(name == chosen) %>%
    select(files) %>% extract2(1) %>% extract2(1)
if(is.null(safe_res$error)) {
    files_urls <- safe_res$result %>%
        filter(!text %in% c("Parent Directory", "Index")) %>%
        select(hrefs) %>% unlist
    res <- map2(files_urls, map(files_urls, basename), curl_fetch_disk)
    res
}
