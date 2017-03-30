#' UCI metadata.
#'
#' A tibble containing the metadata of the 360 UCI datasets.
#'
#' @format A tibble with 360 rows and 11 variables:
#' \describe{
#'   \item{name}{Name of the UCI dataset}
#'   \item{data_types}{Data types}
#'   \item{default_task}{Default task}
#'   \item{attribute_types}{Attribute types}
#'   \item{instances}{Number of instance}
#'   \item{attributes}{Number of attributes}
#'   \item{year}{Year}
#'   \item{url}{Dataset URL}
#'   \item{folder}{Data Folder URL}
#'   \item{description}{Data Description URL}
#'   \item{files}{Data files URLs}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets.html}
"uci_metadata"
