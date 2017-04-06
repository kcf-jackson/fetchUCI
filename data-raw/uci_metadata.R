# rm(list = ls())
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

# A function to remove special characters from the dataset name
special_char_trim <- function(str0) {
  # check the encoding
  str1 <- str0 %>% strsplit("") %>% magrittr::extract2(1)
  str_encoding <- str1 %>% map_chr(Encoding)
  if (length(unique(str_encoding)) == 1) {
    return(str_replace_all(str1, "[[:punct:]]", ""))
  }
  
  # If there is mixed encoding, then find the majority one and remove all the rest
  encoding_table <- table(str_encoding)
  majority <- which(encoding_table == max(encoding_table)) %>% names()
  str2 <- str1[str_encoding == majority] %>% paste(collapse = "")
  
  # remove punctuation
  str2 %>% str_replace_all("[[:punct:]]", "")
}


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
uci_data_urls$name %<>% map_chr(special_char_trim)

# Join the two tibbles then save.
uci_metadata %<>% left_join(., uci_data_urls)
devtools::use_data(uci_metadata, overwrite = TRUE)
