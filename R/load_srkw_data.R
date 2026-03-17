#' Load Southern Resident Killer Whale Population Data
#'
#' Loads the data set containing yearly census counts for
#' Southern Resident killer whales (Orcinus orca). The data set includes yearly
#' births, mortality, total population counts and J,K,L pod counts.
#'
#' @details
#' The data were provided by the Center for Whale Research (CWR) and National
#' Oceanic and Atmospheric Administration (NOAA).
#'
#' @return A data frame including SRKW population data
#'
#' @examples
#' data <- load_srkw_data()
#' head(data)
#'
#' @export
load_srkw_data <- function() {

  data_path <- system.file(
    "extdata",
    "srkw_population.csv",
    package = "srorcapop"
  )

  utils::read.csv(data_path)

}

