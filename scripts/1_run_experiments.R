#***************************************************************************************************
#
#  Script for reproducing AVM Uncertainty analysis
#
#***************************************************************************************************

## MAKE SURE YOU'VE TESTED YOUR SYSTEM WITH THE SANDBOX.R FILE FIRST!

### Setup ------------------------------------------------------------------------------------------

  # Load Libraries
  library(kingCoData) #devtools::install_github('anonymousREAuthor/kingCoData')
  library(hpiR)
  library(tidyverse)
  library(avmUncertainty) #devtools::install_github('anonymousREAuthor/avmUncertainty')

  # Create directory for results if it doesn't exist
  dir.create(file.path(getwd(), 'results'))
  setwd(file.path(getwd(), 'results'))

  # Load data from packages
  data(kingco_sales, package = 'kingCoData')
  data(submarket_df, package = 'avmUncertainty')

### Data and Model Prep ----------------------------------------------------------------------------

 ## Data Prep

  # Add submarket designations to data
  sales_df <- kingco_sales %>%
    dplyr::left_join(., submarket_df, by = 'area')

  # Combine bathrooms and sum up views, create wfnt and townhomes
  raw_df <- raw_df %>%
    dplyr::mutate(baths = bath_full + (.75 * bath_3qtr) + (.5 * bath_half),
                  view_score = view_rainier + view_olympics + view_cascades + view_territorial +
                    view_skyline + view_sound + view_lakewash + view_lakesamm,
                  wfnt = ifelse(wfnt == 0, 0, 1),
                  townhome = ifelse(present_use == 29, 1, 0))

  # Remove obvious bad homes
  sales_df <- raw_df %>%
    dplyr::filter(sqft > 400 &
                    baths > 0 &
                    beds > 0 &
                    beds < 33 &
                    sqft_1 <= sqft_lot &
                    year_built <= lubridate::year(sale_date))
  bad_count <- nrow(raw_df) - nrow(sales_df)
  saveRDS(sales_df, file.path(getwd(), paste0('sales_data.rds')))

  # Create filtered data for sensitivity test

  # Remove all those with strange home fact histories
  filter_df <- sales_df %>%
    dplyr::filter(join_status %in% c('new', 'nochg'))

  # Within each year, remove sales outside of middle 95th quantile
  filter_df <-
    purrr::map(.x = split(filter_df, substr(filter_df$sale_date, 1, 4)),
               .f = trimPriceQuantiles,
               lo_qtl = .10,
               hi_qtl = .90) %>%
    dplyr::bind_rows()
  saveRDS(filter_df, file.path(getwd(), paste0('filtered_data.rds')))

  ## Modeling Prep

  # Set intervals
  test_intervals <- c(.5, .68, .80, .95)

  # Set experiment dates
  pred_dates <- seq(as.Date("2000-01-01"), length = 20 * 12, by = "1 month")

  # Set linear model specification
  lm_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + latitude + longitude +
                          as.factor(trans_period))

  # Set non-linear model specification (random forest)
  rf_spec <- as.formula(log(adj_price) ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + latitude + longitude)

### Main Experiment --------------------------------------------------------------------------------

  ## Global

  # Linear Model
  lm_glob <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'lm',
                          model_spec = lm_spec,
                          boot_samples = 100,
                          pred_intervals = test_intervals)
  saveRDS(lm_glob, file.path(getwd(), 'lm_glob.RDS'))

  ## Non-linear
  rf_glob <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'rf',
                          model_spec = rf_spec,
                          pred_intervals = test_intervals)
  saveRDS(rf_glob, file.path(getwd(), 'rf_glob.RDS'))

  ## Submarkets

  # Linear Model
  lm_subm <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'lm',
                          model_spec = lm_spec,
                          boot_samples = 100,
                          split_field = 'submarket',
                          pred_intervals = test_intervals)
  saveRDS(lm_subm, file.path(getwd(), 'lm_subm.RDS'))

  ## Non-linear
  rf_subm <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'rf',
                          model_spec = rf_spec,
                          split_field = 'submarket',
                          pred_intervals = test_intervals)
  saveRDS(rf_subm, file.path(getwd(), 'rf_subm.RDS'))

  ## Summarize Results into an object for paper/plots

  summ_obj <- list(lmg = lm_glob$calib,
                   lml = lm_subm$calib,
                   rfg = rf_glob$calib,
                   rfl = rf_subm$calib)
  saveRDS(summ_obj, file.path(getwd(), 'summ_obj.RDS'))

### Sensitivity Experiment -------------------------------------------------------------------------

  ## Global

  # Linear Model
  lm_glob_clean <- rollingPreds(data_df =  filter_df,
                               roll_dates = pred_dates[1:2],
                               model_type = 'lm',
                               model_spec = lm_spec,
                               boot_samples = 100,
                               pred_intervals = test_intervals)
  saveRDS(lm_glob_sens, file.path(getwd(), 'lm_glob_clean.RDS'))

  ## Non-linear
  rf_glob_clean <- rollingPreds(data_df =  filter_df,
                               roll_dates = pred_dates,
                               model_type = 'rf',
                               model_spec = rf_spec,
                               pred_intervals = test_intervals)
  saveRDS(rf_glob_clean, file.path(getwd(), 'rf_glob_clean.RDS'))

  ## Submarkets

  # Linear Model
  lm_subm_clean <- rollingPreds(data_df =  filter_df,
                               roll_dates = pred_dates,
                               model_type = 'lm',
                               model_spec = lm_spec,
                               split_field = 'submarket',
                               boot_sampleas = 100,
                               pred_intervals = test_intervals)
  saveRDS(lm_subm_clean, file.path(getwd(), 'lm_subm_clean.RDS'))

  ## Non-linear
  rf_subm_clean <- rollingPreds(data_df =  filter_df,
                                roll_dates = pred_dates,
                                model_type = 'rf',
                                model_spec = rf_spec,
                                split_field = 'submarket',
                                pred_intervals = test_intervals)
  saveRDS(rf_subm_clean, file.path(getwd(), 'rf_subm_clean.RDS'))

  ## Summarize objects from later analysis
  summ_obj_clean <- list(lmg = lm_glob_clean$calib,
                         lml = lm_subm_clean$calib,
                         rfg = rf_glob_clean$calib,
                         rfl = rf_subm_clean$calib)
  saveRDS(summ_obj_clean, file.path(getwd(), 'summ_obj_clean.RDS'))

#***************************************************************************************************
#***************************************************************************************************
