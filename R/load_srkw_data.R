#' Load Southern Resident killer whale population data
#'
#' Loads the data set containing yearly census counts for
#' Southern Resident killer whales
#'
#' @return A data frame including SRKW population data
#' @importFrom utils read.csv
#' @export
load_srkw_data <- function() {

  data_path <- system.file(
    "extdata",
    "srkw_population.csv",
    package = "srorcapop"
  )

  read.csv(data_path)

}
