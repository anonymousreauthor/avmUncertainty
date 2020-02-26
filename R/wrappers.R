#***************************************************************************************************
#
#  Wrapper functions for supporting uncertainty/calibration research
#
#***************************************************************************************************

### Wrappers ---------------------------------------------------------------------------------

#'
#' Wrapper for train/score/calibrate sequence with splitting by a field
#'
#' @param data_df A dataframe of transactions
#' @param split_date Date to split for train/test
#' @param model_type Type of model (current just 'lm' or 'rf')
#' @param model_spec Model specification
#' @param split_field [NULL] Field on which to split up the data
#' @importFrom purrr map transpose
#' @importFrom dplyr bind_rows
#' @return full uncertainty object
#' @export

calibrationWrapper <- function(data_df,
                               split_date,
                               model_type,
                               model_spec,
                               split_field = NULL,
                               ...) {

  if (!is.null(split_field)){
    segment_ <- split(data_df, data_df[[split_field]])
  } else {
    segment_ <- list(data_df)
  }

  sobj <- tobj <- list()

  for(k in 1:length(segment_)){

    if (is.null(length(names(segment_)[[k]]))){
      cat('global\n')
    } else {
      cat(' ', names(segment_)[[k]])
    }

    # Add time adjustments
    study_df <- timeAdjustData(segment_[[k]])

    # Split into train and test
    data_ <- splitData(study_df, as.Date(split_date))

    # Train
    tobj[[k]] <- trainModel(train_df = data_$train,
                            mod_type = model_type,
                            mod_spec = model_spec)

    # Score
    sobj[[k]] = scoreModel(mod_obj = tobj[[k]],
                           test_df = data_$test)

  }

  # Check for NULLs
  del_null <- which(unlist(lapply(sobj, function(x) class(x)[1])) == 'NULL')
  if(length(del_null) > 0) sobj <- sobj[-del_null]

  # Extract the scored objects
  scored_obj <- list(score = purrr::map(.x = sobj, .f = function(x) x$score) %>%
                       dplyr::bind_rows(),
                     predintervals = purrr::map(.x = sobj, .f = function(x) x$predintervals) %>%
                       purrr::transpose() %>%
                       purrr::map(., .f = bind_rows),
                     intervals = sobj[[1]]$intervals) %>%
    structure(., class = 'scored')

  # Calibrate
  calib_obj <- calibrateModel(scored_obj = scored_obj)

  list(score = scored_obj,
       calib = calib_obj) %>%
    structure(., class = 'uncertainty')
}

#'
#' Wrapper to roll through a number of dates and do foreward casts
#'
#' @param data_df Raw data
#' @param roll_dates Datest to evaluate model on
#' @importFrom dplyr filter mutate
#' @importFrom purrr map transpose
#' @return Test data and model calibrations
#' @export

rollingPreds <- function(data_df,
                         roll_dates,
                         model_type,
                         model_spec,
                         keep_results = FALSE,
                         ...){


  results_ <- vector('list', length(roll_dates))

  # Limit Study set
  for (k in 1:length(roll_dates)){

    k_date <- as.Date(roll_dates[k], origin = '1970-01-01')

    message(paste0('\n', model_type, ' - ', as.character(k_date)))

    train_start <- as.Date(k_date - 365, origin = '1970-01-01')
    score_end <- as.Date(k_date + 30, origin = '1097-01-01')

    roll_df <- data_df %>%
      dplyr::filter(sale_date >= train_start &
                      sale_date < score_end)

    results_[[k]] <- calibrationWrapper(data_df = roll_df,
                                        split_date = k_date,
                                        model_type = model_type,
                                        model_spec = model_spec,
                                        ...)$score
  }

  # Extract the scored objects
  scored_obj <- list(score = purrr::map(.x = results_, .f = function(x) x$score) %>%
                       dplyr::bind_rows(),
                     predintervals = purrr::map(.x = results_, .f = function(x) x$predintervals) %>%
                       purrr::transpose() %>%
                       purrr::map(., .f = bind_rows),
                     intervals = results_[[1]]$intervals) %>%
    structure(., class = 'scored')

  # Calibrate
  calib_obj <- calibrateModel(scored_obj = scored_obj)

  list(score = scored_obj,
       calib = calib_obj) %>%
    structure(., class = 'uncertainty')
}

#############################################################################################
