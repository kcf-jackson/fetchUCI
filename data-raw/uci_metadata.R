rm(list = ls())
library(curl)
library(dplyr)
library(magrittr)
library(purrr)
library(rvest)
library(stringr)
library(tibble)
library(tidyr)

# A helper function to fetch href from url.
get_href <- function(url, css, dirname = TRUE) {
    pdir <- ifelse(dirname, dirname(url), url)

    atags <- url %>% read_html %>% html_nodes(css)
    text <- atags %>% html_text()
    href <- atags %>% html_attr("href") %>% file.path(pdir, .)

    return(tibble(text = text, href = href))
}

safe_get_href <- safely(get_href)

# Get datasets metadata.
uci_url <- "https://archive.ics.uci.edu/ml/datasets.html"

uci_metadata <- get_href(uci_url, "table tr td table tr td p") %>%
    select(text) %>%
    magrittr::extract2(1) %>%
    magrittr::extract(25:2544) %>%
    matrix(ncol = 7, byrow = TRUE) %>%
    set_colnames(
        c("name", "data_types", "default_task", "attribute_types", "instances",
          "attributes", "year")) %>%
    as_tibble() %>%
    mutate_each(funs(str_trim)) %>%
    mutate_each_(funs(factor),
                 c("data_types", "default_task", "attribute_types")) %>%
    mutate_each_(funs(as.numeric),
                 c("instances", "attributes", "year"))

# Get data folder and data description urls.
uci_data_urls <- get_href(uci_url, "table p b a") %>%
    rename(name = text, url = href)
uci_data_urls %<>% mutate(
        download_hrefs = map(url, get_href, "table tr td table tr td p a")) %>%
    unnest() %>% spread(text, href)

# Get data file urls.
uci_data_urls %<>% mutate(
    file_urls = map(`Data Folder`, safe_get_href, "body table tr td a",
                    dirname = FALSE))
uci_data_urls %<>%
    select(name, url, `Data Folder`, `Data Set Description`, file_urls) %>%
    set_colnames(c("name", "url", "folder", "description", "files"))

# Join the two tibbles then save.
uci_metadata %<>% left_join(., uci_data_urls)
devtools::use_data(uci_metadata, overwrite = TRUE)