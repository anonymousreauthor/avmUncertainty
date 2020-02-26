#***************************************************************************************************
#
#  Script for reproducing AVM Uncertainty analysis
#
#***************************************************************************************************

## MAKE SURE YOU'VE TESTED YOUR SYSTEM WITH THE SANDBOX.R FILE FIRST!

### Setup ------------------------------------------------------------------------------------------

  # Load Libraries
  library(kingCoData)
  library(hpiR)
  library(tidyverse)
  library(avmUncertainty)

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

  # Combine bathrooms and sum up views
  sales_df <- sales_df %>%
    dplyr::mutate(baths = bath_full + (.75 * bath_3qtr) + (.5 * bath_half),
                  view_score = view_rainier + view_olympics + view_cascades + view_territorial +
                    view_skyline + view_sound + view_lakewash + view_lakesamm,
                  wfnt = ifelse(wfnt == 0, 0, 1))

  # Create filtered data for sensitivity test

  # Remove all those with strange home fact histories
  filter_df <- sales_df %>%
    dplyr::filter(join_status %in% c('new', 'nochg'))

  # Within each year, remove sales outside of middle 95th quantile
  filter_df <-
    purrr::map(.x = split(filter_df, substr(filter_df$sale_date, 1, 4)),
               .f = trimPriceQuantiles,
               lo_qtl = .25,
               hi_qtl = .75) %>%
    dplyr::bind_rows()

  ## Modeling Prep

  # Set experiment dates
  pred_dates <- seq(as.Date("2000-01-01"), length = 20 * 12, by = "1 month")

  # Set linear model specification
  lm_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + latitude + longitude +
                          as.factor(trans_period))

  # Set non-linear model specification (random forest)
  rf_spec <- as.formula(adj_price ~ year_built + sqft + grade + condition + beds + baths +
                          sqft_lot + view_score + wfnt + latitude + longitude)

### Main Experiment --------------------------------------------------------------------------------

  ## Global

  # Linear Model
  lm_glob <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'lm',
                          model_spec = lm_spec,
                          boot_samples = 100)
  saveRDS(lm_glob, file.path(getwd(), 'lm_glob.RDS'))

  ## Non-linear
  rf_glob <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'rf',
                          model_spec = rf_spec)
  saveRDS(rf_glob, file.path(getwd(), 'rf_glob.RDS'))

  ## Submarkets

  # Linear Model
  lm_subm <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'lm',
                          model_spec = lm_spec,
                          split_field = 'submarket')
  saveRDS(lm_subm, file.path(getwd(), 'lm_subm.RDS'))

  ## Non-linear
  rf_subm <- rollingPreds(data_df =  sales_df,
                          roll_dates = pred_dates,
                          model_type = 'rf',
                          model_spec = rf_spec,
                          split_field = 'submarket')
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
                               boot_samples = 100)
  saveRDS(lm_glob_sens, file.path(getwd(), 'lm_glob_clean.RDS'))

  ## Non-linear
  rf_glob_clean <- rollingPreds(data_df =  filter_df,
                               roll_dates = pred_dates,
                               model_type = 'rf',
                               model_spec = rf_spec)
  saveRDS(rf_glob_clean, file.path(getwd(), 'rf_glob_clean.RDS'))

  ## Submarkets

  # Linear Model
  lm_subm_clean <- rollingPreds(data_df =  filter_df,
                               roll_dates = pred_dates[1:2],
                               model_type = 'lm',
                               model_spec = lm_spec,
                               split_field = 'submarket',
                               boot_sampleas = 100)
  saveRDS(lm_subm_clean, file.path(getwd(), 'lm_subm_clean.RDS'))

  ## Non-linear
  rf_subm_clean <- rollingPreds(data_df =  filter_df,
                                roll_dates = pred_dates,
                                model_type = 'rf',
                                model_spec = rf_spec,
                                split_field = 'submarket')
  saveRDS(rf_subm_clean, file.path(getwd(), 'rf_subm_clean.RDS'))

  ## Summarize objects from later analysis
  summ_obj_clean <- list(lmg = lm_glob_clean$calib,
                         lml = lm_subm_clean$calib,
                         rfg = rf_glob_clean$calib,
                         rfl = rf_subm_clean$calib)
  saveRDS(summ_obj_clean, file.path(getwd(), 'summ_obj_clean.RDS'))

### Sensitity: Very Clean Data ---------------------------------------------------------------------

  # Load results if not already present
  if (!exists('lm_glob')) lm_glob <- readRDS(file.path(getwd(), 'lm_glob.RDS'))
  if (!exists('lm_subm')) lm_subm <- readRDS(file.path(getwd(), 'lm_subm.RDS'))
  if (!exists('rf_glob')) rf_glob <- readRDS(file.path(getwd(), 'rf_glob.RDS'))
  if (!exists('rf_subm')) rf_subm <- readRDS(file.path(getwd(), 'rf_subm.RDS'))

   ### Error_df
   error_df <- dplyr::bind_rows(
     lm_glob$score$score %>% dplyr::select(trans_id, pred, log_error),
     lm_subm$score$score %>% dplyr::select(trans_id, pred, log_error),
     rf_glob$score$score %>% dplyr::select(trans_id, pred, log_error),
     rf_subm$score$score %>% dplyr::select(trans_id, pred, log_error)) %>%
     dplyr::group_by(trans_id) %>%
     dplyr::summarize(error = mean(log_error),
                      pred = mean(pred))

   set.seed(1)
   perturb_df <- sales_df %>%
     dplyr::mutate(orig_price = sale_price,
                   sale_price = exp(log(sale_price) + sample(error_df$error, nrow(.),
                                                             replace = TRUE)))

   ## Global

   # Linear Model
   lm_glob_dirty <- rollingPreds(data_df =  perturb_df,
                                 roll_dates = pred_dates,
                                 model_type = 'lm',
                                 model_spec = lm_spec,
                                 boot_samples = 100)
   saveRDS(lm_glob_dirty, file.path(getwd(), 'lm_glob_dirty.RDS'))

   ## Non-linear
   rf_glob_dirty <- rollingPreds(data_df =  perturb_df,
                                 roll_dates = pred_dates,
                                 model_type = 'rf',
                                 model_spec = rf_spec)
   saveRDS(rf_glob_dirty, file.path(getwd(), 'rf_glob_dirty.RDS'))

   ## Submarkets

   # Linear Model
   lm_subm_dirty <- rollingPreds(data_df =  perturb_df,
                                 roll_dates = pred_dates,
                                 model_type = 'lm',
                                 model_spec = lm_spec,
                                 split_field = 'submarket')
   saveRDS(lm_subm_dirty, file.path(getwd(), 'lm_subm_dirty.RDS'))

   ## Non-linear
   rf_subm_dirty <- rollingPreds(data_df =  perturb_df,
                                 roll_dates = pred_dates,
                                 model_type = 'rf',
                                 model_spec = rf_spec,
                                 split_field = 'submarket')
   saveRDS(rf_subm_dirty, file.path(getwd(), 'rf_subm_dirty.RDS'))

   ## Summarize objects from later analysis
   summ_obj_dirty <- list(lmg = lm_glob_dirty$calib,
                          lml = lm_subm_dirty$calib,
                          rfg = rf_glob_dirty$calib,
                          rfl = rf_subm_dirty$calib)
   saveRDS(summ_obj_dirty, file.path(getwd(), 'summ_obj_dirty.RDS'))

#***************************************************************************************************
#***************************************************************************************************
