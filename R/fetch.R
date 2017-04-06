utils::globalVariables(c("name", "files", ".", "href", "description"))

#' Fetch UCI data description
#' @importFrom magrittr "%>%"
#' @export
#' @param uci_dataset_name Name of UCI dataset. See fetchUCI::uci_metadata$name.
#' @param dest Directory in which to create a folder. Default to "./".
#' @examples
#' \dontrun{
#' fetch_description("Abalone")
#' }
fetch_description <- function(uci_dataset_name, dest = ".") {
    desc_url <- fetchUCI::uci_metadata %>%
        dplyr::filter(name == uci_dataset_name) %>%
        dplyr::select(description) %>% purrr::as_vector()

    if (desc_url != "https://archive.ics.uci.edu/ml/datasets/#") {
        if (!dir.exists(uci_dataset_name)) {
            dir.create(uci_dataset_name)
        }

        desc_file <- file.path(dest, uci_dataset_name, basename(desc_url))
        desc_url %>% curl::curl_download(desc_file)

        cat(readLines(desc_file), sep = "\n")
    }
}

#' Fetch UCI data files
#' @importFrom magrittr "%>%"
#' @export
#' @param uci_dataset_name Name of UCI dataset. See fetchUCI::uci_metadata$name.
#' @param dest Directory in which to create a folder. Default to "./".
#' @return A list of results from code{curl::curl_fetch_disk}.
#' @examples
#' \dontrun{
#' fetch_data("Abalone")
#' }
fetch_data <- function(uci_dataset_name, dest = ".") {
    safe_res <- fetchUCI::uci_metadata %>%
        dplyr::filter(name == uci_dataset_name) %>%
        dplyr::select(files) %>%
        magrittr::extract2(1) %>%
        magrittr::extract2(1)

    if (is.null(safe_res$error)) {
        if (!dir.exists(uci_dataset_name)) {
            dir.create(uci_dataset_name)
        }

        file_urls <- safe_res$result %>%
            dplyr::filter(!.$text %in% c("Parent Directory", "Index")) %>%
            dplyr::select(href) %>% unlist
        filenames <- purrr::map(file_urls, basename) %>%
            file.path(dest, uci_dataset_name, .)

        res <- purrr::map2(file_urls, filenames, curl::curl_fetch_disk)

        return(res)
    }
}

#' Group fetch data
#' @uci_dataset_name Vector of characters; names of UCI dataset.
#' @param dest Directory in which to create a folder. Default to "./".
#' @keywords internal
#' @description This function prints out the dataset names that are problematic and 
#' requires manual fixing.
batch_fetch_data <- function(uci_dataset_name, dest = ".") {
  if (length(uci_dataset_name) == 1) {
    return(fetch_data(uci_dataset_name, dest))
  } 
  for (model in uci_dataset_name) {
    tryCatch(
      expr = {
        fetch_data(model, dest)
      },
      error = function(e) {
        print(model)
        print(e)
      },
      warning = function(w) {
        print(model)
        print(w)
      }
    )
  }
}
