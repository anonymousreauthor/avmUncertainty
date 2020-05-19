#***************************************************************************************************
#
#  Helper functions for supporting uncertainty/calibration research
#
#***************************************************************************************************

#'
#' Extracts and summarize all data from raw experiment output
#'
#' @param file_name Raw data
#' @param intervals [NULL] Vector of interval levels at which to extract the raw results
#' @importFrom dplyr select bind_rows mutate
#' @importFrom purrr map2
#' @return Summarized extracted data
#' @export

extractResults <- function(file_name,
                           intervals = NULL){

  # Get Labels
  model_class <- ifelse(grepl('lm', file_name), 'lm', 'rf')
  geography <- ifelse(grepl('glob', file_name), 'global', 'local')
  data_condition <- 'standard'
  data_condition <- ifelse(grepl('clean', file_name), 'filtered', data_condition)
  data_condition <- ifelse(grepl('dirty', file_name), 'perturbed', data_condition)

  # Read in data
  cat('Reading in data from ', model_class, ' - ', geography, ' - ', data_condition, '\n')
  raw_obj <- readRDS(file.path(getwd(), 'results', paste0(file_name, '.RDS')))

  # Get Error Data
  error_df <- raw_obj$score$score %>%
    dplyr::select(trans_id, pred, error) %>%
    dplyr::mutate(model_class = model_class,
                  geography = geography,
                  data_condition = data_condition)

  # Get calibration data
  calibration_df <- purrr::map2(.x = raw_obj$calib$accuracy,
                                .y = names(raw_obj$calib$accuracy),
                                .f = function(x, y){
                                  x$data %>%
                                    dplyr::mutate(method = y,
                                                  model_class = model_class,
                                                  geography = geography,
                                                  data_condition = data_condition,
                                                  coverage = round(coverage, 3))
                                }) %>%
    dplyr::bind_rows()

  # Get efficiency data
  efficiency_df <- purrr::map2(.x = raw_obj$calib$efficiency,
                               .y = names(raw_obj$calib$efficiency),
                               .f = function(x, y){
                                 x %>%
                                   dplyr::mutate(method = y,
                                                 model_class = model_class,
                                                 geography = geography,
                                                 data_condition = data_condition)
                               }) %>%
    dplyr::bind_rows()

  if (is.null(intervals)){
    raw_intervals <- NULL
  } else {
    raw_intervals <- purrr::map(.x = as.character(intervals),
                                .f = extractIntervals,
                                raw_obj = raw_obj) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(model_class = model_class,
                    geography = geography,
                    data_condition = data_condition)
  }

  list(accuracy = error_df,
       calibration = calibration_df,
       efficiency = efficiency_df,
       raw_intervals = raw_intervals)
}

#'
#' Extract raw PI values
#'
#' @param raw_obj Raw data object
#' @param interval Interval at which to extract the raw PIs
#' @importFrom dplyr select bind_rows mutate
#' @importFrom purrr map2
#' @importFrom tidyselect contains
#' @return Raw intervals
#' @export

extractIntervals <- function(raw_obj,
                             interval){

  lo_str = 'lo_0.90'
  hi_str = 'hi_0.90'

  purrr::map2(.x = raw_obj$score$predintervals,
              .y = names(raw_obj$score$predintervals),
              interval = interval,
              .f = function(x, y, interval){
                x %>%
                  dplyr::select(trans_id,
                                lo = tidyselect::contains(lo_str),
                                hi = tidyselect::contains(hi_str)) %>%
                  dplyr::mutate(method = y,
                                interval = interval)
              }) %>%
    dplyr::bind_rows()
}

#############################################################################################
