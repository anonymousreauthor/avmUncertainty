#***************************************************************************************************
#
#  Sandbox for looking into uncertainty and calibration methods
#
#***************************************************************************************************

  ## Load Libraries

   #devtools::install_github('anonreauthor/avmUncertainty')
   library(avmUncertainty)
   #devtools::install_github('anonreauthor/kingCoData')
   library(kingCoData)

   library(hpiR)
   library(tidyverse)

  ## Load Data

   data(kingco_sales, package = 'kingCoData')
   data(submarket_df, package = 'avmUncertainty')

### Step by step Example ---------------------------------------------------------------------------------

 ## Prep Data

   # Add submarket
   kingco_sales <- kingco_sales %>%
      dplyr::left_join(., submarket_df, by = 'area')

   # Limit Study set
   trim_df <- kingco_sales %>%
    dplyr::filter(join_status %in% c('new', 'nochg') &
                    sale_date >= as.Date('2017-06-01') &
                     sale_date <= as.Date('2018-06-30'))

   # Add time adjustments
   study_df <- timeAdjustData(trans_df = trim_df)

   # Split into train and test
   data_ <- splitData(study_df,
                      '2018-06-01')

   # Set model specifications
   lm_spec <- as.formula(log(price) ~ year_built + sqft + grade + condition +
                           beds + bath_full + bath_3qtr + bath_half +
                           as.factor(trans_period) + latitude + longitude)

  ## Train Models

   lm_model <- trainModel(train_df = data_$train,
                            mod_type = 'lm',
                            mod_spec = lm_spec)

   ## Score Models

   lm_score <- scoreModel(mod_obj = lm_model,
                          test_df = data_$test,
                          pred_intervals = c(.5, .68, .80, .95))

  ## Calibration Models

   lm_calib <- calibrateModel(scored_obj = lm_score)

  ## Plot calibrations

   plotCalib(lm_calib)

   plotEffic(lm_calib)

#***************************************************************************************************
