#***************************************************************************************************
#
#  Script for summarizing results from uncertainty analysis
#
#***************************************************************************************************

### Setup ------------------------------------------------------------------------------------------

  ## Load Libraries

  library(tidyverse)
  library(moments)
  library(avmUncertainty)
  library(kingCoData)

  # Set Directory
  setwd(file.path(getwd(), 'results'))

  # Names of raw_files
  raw_files <- c('lm_glob', 'rf_glob', 'lm_subm', 'rf_subm',
                 'lm_glob_clean', 'rf_glob_clean', 'lm_subm_clean', 'rf_subm_clean',
                 'lm_glob_dirty', 'rf_glob_dirty', 'lm_subm_dirty', 'rf_subm_dirty')

### Extract the raw data ---------------------------------------------------------------------------

  ## Load in raw results
  summ_obj <- purrr::map(.x = raw_files,
                         .f = extractResults)

  ## Extract row level errors
  error_obj <-purrr::map(.x = summ_obj,
                       .f = function(x) x$accuracy) %>%
    dplyr::bind_rows()

  ## Kurtosis table
  kurt_tbl <- error_obj %>%
    dplyr::group_by(model_class, geography, data_condition) %>%
    dplyr::summarize(kurt = moments::kurtosis(error))

  ## Summarize down as raw is too big for RMD
  acc_tbl <-
    error_obj %>%
    dplyr::group_by(model_class, geography, data_condition) %>%
    dplyr::summarize(count = n(),
                     mape = median(abs(error)),
                     mpe = median(error),
                     pe10 = length(which(abs(error) <= .10)) / count,
                     pe30 = length(which(abs(error) <= .30)) / count) %>%
    data.frame() %>%
    dplyr::mutate_at(., c('mape', 'mpe', 'pe10', 'pe30'), round, digits=3)

  ## Extract calibration results
  cal_obj <- purrr::map(.x = summ_obj,
                        .f = function(x) x$calibration) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(method %in% c('model', 'error')) %>%
    dplyr::mutate(error = coverage - interval)

  ## Extract efficiency results
  eff_obj <-purrr::map(.x = summ_obj,
                       .f = function(x) x$efficiency) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(method %in% c('model', 'error'))

### Data Summary -----------------------------------------------------------------------------------

  data(kingco_sales)
  # Add submarket designations to data
  sales_df <- kingco_sales %>%
    dplyr::left_join(., submarket_df, by = 'area')

  # Combine bathrooms and sum up views
  sales_df <- sales_df %>%
    dplyr::mutate(baths = bath_full + (.75 * bath_3qtr) + (.5 * bath_half),
                  view_score = view_rainier + view_olympics + view_cascades + view_territorial +
                    view_skyline + view_sound + view_lakewash + view_lakesamm,
                  wfnt = ifelse(wfnt == 0, 0, 1),
                  townhome = ifelse(present_use == 29, 1, 0))

  # Remove obvious bad homes
  sales_df <- sales_df %>%
    dplyr::filter(sqft > 400 &
                    baths > 0 &
                    beds > 0 &
                    beds < 33 &
                    sqft_1 <= sqft_lot &
                    year_built <= lubridate::year(sale_date))
  bad_count <- full_count - nrow(sales_df)

  summ_df <-
    sales_df %>%
    dplyr::select(sale_price, sale_date, year_built, sqft, grade, condition, beds, baths, sqft_lot,
                  view_score, wfnt, latitude, longitude) %>%
    lapply(., summary) %>%
    do.call(cbind, .) %>%
    as.data.frame() %>%
    mutate_at(c('sale_price', 'year_built', 'sqft', 'sqft_lot'), round, digits = 0) %>%
    mutate_at(c('grade', 'condition', 'beds', 'baths', 'view_score', 'wfnt',
                'latitude', 'longitude'), round, digits = 2) %>%
    mutate(sale_date = as.Date(sale_date, origin = '1970-01-01')) %>%
    t() %>%
    as.data.frame() %>%
    purrr::set_names(., c('Min', '25th', 'Median' ,'Mean', '75th', 'Max')) %>%
    dplyr::mutate(Variable = rownames(.)) %>%
    dplyr::select(Variable, tidyselect::everything())

### Write_out -----------------------------------------------------------------------------

  results_ <- list(accr = acc_tbl,
                   kurtosis = kurt_tbl,
                   cal = cal_obj,
                   eff = eff_obj,
                   data = summ_df)

 saveRDS(results_, file.path(getwd(), paste0('summary_results.RDS')))

#***************************************************************************************************
