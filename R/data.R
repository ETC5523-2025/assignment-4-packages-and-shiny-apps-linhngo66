#' Water quality monitoring data from Yarra River (Melbourne, Australia)
#'
#' A dataset containing water quality measurements from monitoring sites across Yarra River 
#' (Melbourne, Australia), including various chemical and physical parameters measured 
#' between 1991 and 2025
#'
#' @format A data frame with 1,501 rows and 13 variables:
#' \describe{
#'   \item{site_id}{Factor. Unique identifier for the monitoring site (e.g., 229143)}
#'   \item{name}{Factor. Name of the monitoring site location (e.g., "YARRA @ CHANDLER HWY")}
#'   \item{datetime}{POSIXct. Date and time when the sample was collected}
#'   \item{data_type}{Factor. Type of data collected - 2 levels: Quality and Quantity)}
#'   \item{parameter}{Factor. The water quality parameter measured - 22 levels (e.g., "Total Arsenic")}
#'   \item{value}{Numeric. The measured value of the parameter}
#'   \item{unit_of_measurement}{Character. The unit in which the parameter is measured (e.g., "mg/L")}
#'   \item{year}{Numeric. Year when the sample was collected}
#'   \item{month}{Numeric. Month when the sample was collected (1-12)}
#'   \item{weekday}{Ordered factor. Day of the week when the sample was collected}
#'   \item{period}{Factor. Decade period - 2 levels: 1990s and 2020s)}
#'   \item{season}{Factor. Season when the sample was collected - 4 levels: Autumn, Winter, Spring, Summer}
#'   \item{parameter_type}{Factor. Category of the parameter - 3 levels: Toxicant, Nutrient, Physical/Chemical Stressor}
#' }
#' @source Victorian Department of Energy, Environment, and Climate Action \url{https://data.water.vic.gov.au/WMIS}
#' @examples
#' # Load the dataset
#' data(yarra_water)
#' 
#' # View structure
#' str(yarra_water)
#' 
#' # Summary statistics
#' summary(yarra_water)
#'
"yarra_water"