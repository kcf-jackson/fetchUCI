# Organise UCI datasets in a tibble
get_hrefs <- function(url, css, dirname = TRUE) {
  pdir <- ifelse(dirname, dirname(url), url)
  
  atags <- url %>% read_html %>% html_nodes(css)
  text <- atags %>% html_text()
  hrefs <- atags %>% html_attr("href") %>% file.path(pdir, .)
  
  return(tibble(text = text, hrefs = hrefs))
}

# Function to fetch description
fetch_description <- function(uci_dataset_name, dest = ".") {
  desc_url <- uci_datasets %>% filter(name == uci_dataset_name) %>% 
    select(`Data Set Description`) %>% as_vector
  
  if (desc_url != "https://archive.ics.uci.edu/ml/datasets/#") {
    if (!dir.exists(uci_dataset_name)) {
      dir.create(uci_dataset_name)  
    }
    
    desc_file <- file.path(dest, uci_dataset_name, basename(desc_url))
    desc_url %>% curl_download(desc_file)
    
    cat(readLines(desc_file), sep = "\n")
  }
}

# Function to fetch data
fetch_data <- function(uci_dataset_name, dest = ".") {
  safe_res <- uci_datasets %>% filter(name == uci_dataset_name) %>%
    select(files) %>% extract2(1) %>% extract2(1)
  
  if (is.null(safe_res$error)) {
    if (!dir.exists(uci_dataset_name)) {
      dir.create(uci_dataset_name)  
    }
    
    file_urls <- safe_res$result %>%
      filter(!text %in% c("Parent Directory", "Index")) %>%
      select(hrefs) %>% unlist
    filenames <- map(file_urls, basename) %>% 
      file.path(dest, uci_dataset_name, .)
    
    res <- map2(file_urls, filenames, curl_fetch_disk)
    res
  }
}
