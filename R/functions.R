#***************************************************************************************************
#
#  Functions for supporting uncertainy/calibration research
#
#***************************************************************************************************

### Data prep --------------------------------------------------------------------------------------

#'
#' Time adjust data to current time
#'
#' @param trans_df Data.frame of transactions (from kingCoSales)
#' @return List of training and test data
#' @importFrom hpiR hedIndex
#' @importFrom dplyr left_join select mutate
#' @export

timeAdjustData <- function(trans_df){

  require(hpiR)

  # Create Price Index
  hpi_index <- hedIndex(trans_df = trans_df,
                        date = 'sale_date',
                        price = 'sale_price',
                        trans_id = 'sale_id',
                        periodicity = 'monthly',
                        prop_id = 'pinx',
                        estimator = 'base',
                        log_dep = TRUE,
                        dep_var = 'price',
                        ind_var = c('year_built', 'sqft', 'sqft_1', 'beds', 'grade', 'bath_full'),
                        smooth = TRUE)

  # Extract index adjustment df
  index_df <- data.frame(sale_month = hpi_index$index$period,
                         value = hpi_index$index$smooth,
                         adj = hpi_index$index$smooth /
                           hpi_index$index$smooth[length(hpi_index$index$smooth)])

  # Join index values to transactios adjust to last period value
  hpi_index$data %>%
    as.data.frame() %>%
    dplyr::mutate(trans_period = as.integer(trans_period)) %>%
    dplyr::left_join(., index_df %>%
                       dplyr::select(trans_period = sale_month, adj) %>%
                       dplyr::mutate(trans_period = as.integer(trans_period),
                                     adj = as.numeric(adj)),
                     by = 'trans_period') %>%
    dplyr::mutate(adj_price = price / adj)
}

#'
#' Create train/test split
#'
#' Split data based on a horizon ate
#'
#' @param study_df A dataframe of transactions
#' @param horizon_date Date after which to set aside as test data
#' @importFrom dplyr filter mutate
#' @return List of training and test data
#' @export

splitData <- function(study_df,
                      horizon_date){

  train_df <- study_df %>%
    dplyr::filter(sale_date < as.Date(horizon_date, origin = '1970-01-01'))

  test_df <- study_df %>%
    dplyr::filter(sale_date >= as.Date(horizon_date, origin = '1970-01-01')) %>%
    dplyr::mutate(trans_period = max(train_df$trans_period))

  list(train = train_df,
       test = test_df)
}

#'
#' Create train/test split
#'
#' Split data based on a horizon ate
#'
#' @param x_df Data.frame to trim from
#' @param lo_qtl Lower quantile cutoff
#' @param hi_qtl Higher quantile cutoff
#' @importFrom dplyr filter
#' @return Filtered sales data
#' @export

trimPriceQuantiles <- function(x_df,
                               lo_qtl,
                               hi_qtl){

  lo <- quantile(x_df$sale_price, lo_qtl)
  hi <- quantile(x_df$sale_price, hi_qtl)
  x_df %>%
    dplyr::filter(sale_price > lo & sale_price < hi)
}

# Train Models -------------------------------------------------------------------------------------

#'
#' Train models
#'
#' Generic method for training models
#'
#' @param train_df Training data
#' @param mod_type Type of model to train
#' @param mod_spec Model specification
#' @param ... Additional arguments
#' @return Trained model object
#' @export

trainModel <- function(train_df,
                       mod_type,
                       mod_spec,
                       ...){

  mod_type <- structure(mod_type, class = mod_type)
  UseMethod('trainModel', mod_type)
}

#'
#' Train a linear model
#'
#' @method trainModel lm
#' @inherit trainModel params
#' @importFrom stats lm
#' @return Trained linear model plut optional boot strapped approach
#' @export

trainModel.lm <- function(train_df,
                          mod_type,
                          mod_spec,
                          boot_cnt = NULL,
                          boot_per = 0.5,
                          ...){

  full_model <- lm(mod_spec, data = train_df)

  if (is.null(boot_cnt)){
    boot_ <- NULL
  } else {
    boot_ <- vector(mode = 'list', length = boot_cnt)

    for (k in 1:boot_cnt){
      idx <- sample(1:nrow(train_df), round(nrow(train_df) * boot_per, 0), TRUE)
      boot_[[k]] <- lm(mod_spec, data = train_df[idx, ])
    }
  }

  train_df <- crossVal(train_df, mod_spec, ...)

  structure(list(full = full_model,
                 boot = boot_,
                 data = train_df),
            class = mod_type)
}

#'
#' Cross validation for linear model
#'
#' @param train_df Training data
#' @param mod_spec Model specification
#' @param k [5] Number of folds
#' @importFrom caret createFolds
#' @importFrom dplyr bind_rows
#' @return Cross validation prediction
#' @export

crossVal <- function(train_df,
                     mod_spec,
                     k=5){

  require(caret)

  k_folds <- caret::createFolds(y=1:nrow(train_df),
                                k=k,
                                list=TRUE,
                                returnTrain=FALSE)
  res_ <- vector('list', k)

  for (i in 1:k){
    i_k <- k_folds[[i]]
    mod <- lm(mod_spec, data = train_df[-i_k, ])
    i_k <- i_k[which(train_df$trans_period[i_k] %in%
                       train_df$trans_period[-i_k])]
    res_[[i]] <- data.frame(trans_id = train_df$trans_id[i_k],
                            cv_pred = predict(mod, train_df[i_k, ]))
    if (all(is.na(res_[[i]]$cv_pred))) res_[[i]] <- NULL
  }

  train_df %>%
    dplyr::left_join(., res_ %>% dplyr::bind_rows(), by = 'trans_id')
}

#'
#' Train a random forest model
#'
#' @method trainModel rf
#' @inherit trainModel params
#' @importFrom ranger ranger
#' @return Trained linear model plut optional boot strapped approach
#' @export

trainModel.rf <- function(train_df,
                          mod_type,
                          mod_spec,
                          ...){

  full_model <- ranger::ranger(mod_spec,
                               data = train_df,
                               quantreg = TRUE,
                               ...)

  train_df$cv_pred <- full_model$predictions

  structure(list(full = full_model,
                 boot = NULL,
                 data = train_df),
            class = c(mod_type, 'train'))
}

# Score Models -------------------------------------------------------------------------------------

#'
#' Score models
#'
#' Wrapper for scoring any model class with any set of pi methods and pred intervals
#'
#' @param mod_obj A model object from trainModel()
#' @param test_df Data.frame of data to score
#' @param pi_methods [c('model', 'error', 'instance')] Different methods for producing pred intervals
#' @param pred_intervals [.05 to .95 by .05] Desired prediction intervals
#' @param ... Additional arguments
#' @importFrom purrr map set_names
#' @return Scored model object
#' @export

scoreModel <- function(mod_obj,
                       test_df,
                       pi_methods = c('model', 'error'),
                       pred_intervals = seq(.05, .95, by =.05),
                       ...){

  if (nrow(test_df) <= 1) return(NULL)

  # Point Prediction
  point_df <- data.frame(trans_id = test_df$trans_id,
                         pred = pointPrediction(mod_obj = mod_obj,
                                                test_df = test_df,
                                                pred_intervals = pred_intervals))

  if (all(is.na(point_df$pred))) return(NULL)
  test_df$pred <- point_df$pred[match(test_df$trans_id, point_df$trans_id)]
  test_df$error <- test_df$pred - log(test_df$price)

  # Calc Errors
  mod_obj$data <- calculateErrors(mod_obj$data)

  # Calc FSD (if required)
  fsd_obj <- calculateFSD(train_df = mod_obj$data,
                          pred_intervals = pred_intervals,
                          pi_methods = pi_methods)

  # Create PIs for all methods
  pi_ <- purrr::map(.x = pi_methods,
                    .f = scoreInterval,
                    pred_intervals = pred_intervals,
                    test_df = test_df,
                    mod_obj = mod_obj,
                    fsd_obj = fsd_obj,
                    ...) %>%
    purrr::set_names(., pi_methods)

  pi_ <- pi_[unlist(lapply(pi_, nrow)) > 0]

  # Create a unified scored object
  list(score = test_df,
       predintervals = pi_,
       intervals = pred_intervals) %>%
    structure(., class = 'scored')
}

#'
#' Make point predictions
#'
#' Generic method for point predictions of various model classees
#'
#' @param mod_obj A model object from trainModel()
#' @param test_df Data.frame of data to score
#' @param pred_intervals [.05 to .95 by .05] Desired prediction intervals
#' @param ... Additional arguments
#' @return Scored model object
#' @export

pointPrediction <- function(mod_obj,
                            test_df,
                            pred_intervals,
                            ...){

  UseMethod('pointPrediction', mod_obj)
}

#'
#' Point prediction from a linear model
#'
#' @method pointPrediction lm
#' @inherit pointPrediction params
#' @importFrom stats median
#' @export

pointPrediction.lm <- function(mod_obj,
                               test_df,
                               pred_intervals,
                               ...){

  pred <- predict(object = mod_obj$full,
                  newdata = test_df)

  if (all(is.na(pred))) return(NULL)
  pred
}

#'
#' Point Prediction from a random forest model
#'
#' @method pointPrediction rf
#' @inherit pointPrediction params
#' @importFrom stats median
#' @export

pointPrediction.rf <- function(mod_obj,
                               test_df,
                               pred_intervals,
                               ...){

  pred <- as.numeric(predict(object = mod_obj$full,
                             data = test_df,
                             type = 'quantiles',
                             quantiles = .5)$predictions)

  if (all(is.na(pred))) return(NULL)
  pred
}

#'
#' Calculate the prediction error on the test set
#'
#' @param train_df Data.frame with point prediction
#' @importFrom dplyr left_join mutate
#' @return Test observation with prediction error
#' @export

calculateErrors <- function(train_df){

  train_df %>%
    dplyr::mutate(error = cv_pred - log(price))

}

#'
#' Calculate the FSD of test set
#'
#' Includes the various normal distribution expansion at the various prediction intervals
#'
#' @param test_df Data.frame of data to score
#' @param pred_intervals Intervals to calculate bounds at
#' @param pi_methods Methods to use (if 'error' then do, else return NULL)
#' @importFrom stats sd qnorm
#' @return List with FSD and FSD bounds
#' @export

calculateFSD <- function(train_df,
                         pred_intervals,
                         pi_methods){

  if (!'error' %in% pi_methods) return(NULL)

  fsd <- stats::sd(train_df$error)

  # Get SD multipliers
  fsdintervals_ <- list()
  for (k in 1:length(pred_intervals)) {
    cp <- pred_intervals[[k]]
    fsdintervals_[[k]] <- stats::qnorm(.5 + cp / 2) * fsd
  }

  list(fsd = fsd,
       intervals = data.frame(interval = pred_intervals,
                              value = unlist(fsdintervals_))) %>%
    structure(., class = 'fsd')
}

#'
#' Score the prediction intervals
#'
#' Generic method for creating prediction intervals from various methods
#'
#' @param pi_method Method to use for interval construction
#' @param pred_intervals [.05 to .95 by .05] Desired prediction intervals
#' @param test_df Data.frame of data to score
#' @param mod_obj Trained model objects
#' @param fsd_obj [NULL] fsb object from calculateFSD()
#' @param ... Additional arguments
#' @return Scored model object
#' @export

scoreInterval <- function(pi_method,
                          pred_intervals,
                          test_df,
                          mod_obj,
                          fsd_obj = NULL,
                          ...){

  pi_method <- structure(pi_method, class = pi_method)
  UseMethod('scoreInterval', pi_method)
}

#'
#' Score intervals with error method
#'
#' @method scoreInterval error
#' @inherit scoreInterval params
#' @export

scoreInterval.error <- function(pi_method,
                                pred_intervals,
                                test_df,
                                mod_obj,
                                fsd_obj = NULL,
                                ...){

  pi_df <- test_df[, c('trans_id', 'price', 'pred', 'error')]

  for (k in 1:nrow(fsd_obj$intervals)) {

    lo <- pi_df$pred - fsd_obj$intervals$value[[k]]
    hi <- pi_df$pred + fsd_obj$intervals$value[[k]]

    pi_df[[paste0('lo_', format(fsd_obj$intervals$interval[k], nsmall = 2))]] <- lo
    pi_df[[paste0('hi_', format(fsd_obj$intervals$interval[k], nsmall = 2))]] <- hi
  }

  pi_df %>%
    structure(., class = c('error', 'interval_df', 'tbl_df', 'tbl', 'data.frame'))
}

#'
#' Score intervals with model method
#'
#' @method scoreInterval model
#' @inherit scoreInterval params
#' @export

scoreInterval.model <- function(pi_method,
                                pred_intervals,
                                test_df,
                                mod_obj,
                                fsd_obj = NULL,
                                ...){

  rangePrediction(mod_obj = mod_obj,
                  test_df = test_df,
                  pred_intervals = pred_intervals,
                  ...) %>%
    structure(., class = c('model', 'interval_df', 'tbl_df', 'tbl', 'data.frame'))
}

#'
#' Score intervals with data method
#'
#' @method scoreInterval dissimilarity
#' @inherit scoreInterval params
#' @importFrom solitude isolationForest
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows mutate
#' @export

scoreInterval.dissimilarity <- function(pi_method,
                                        pred_intervals,
                                        test_df,
                                        mod_obj,
                                        fsd_obj = NULL,
                                        iso_fields = c('sqft', 'bath_full', 'beds','land_val'),
                                        ...){

  require(solitude)

  # Train model (with train only)
  set.seed(1)
  iso_train <- mod_obj$data[, iso_fields]
  iso_mod <- solitude::isolationForest$new(sample_size = min(256, nrow(iso_train - 10)))
  suppressMessages(iso_mod$fit(iso_train))

  # Score both Train and Test
  suppressMessages(tiso_df <- iso_mod$predict(test_df[, iso_fields]))
  suppressMessages(triso_df <- iso_mod$predict(mod_obj$data[, iso_fields]))

  # Standardize depths (round to integer)
  st_depths <- round(c(tiso_df$average_depth, triso_df$average_depth),  0)
  st_depths <- 1 + (st_depths - min(st_depths))

  # Fix cases with <5 at a given depth (<1 breaks the SD calcs)
  depth_table <- table(st_depths)
  mid <- length(depth_table) / 2
  bad_table <- which(depth_table < 5)
  if (any(bad_table < mid)){
    low_bad <- bad_table[bad_table < mid]
    st_depths[st_depths %in% as.numeric(names(low_bad))] <-
      min(unique(st_depths)[!unique(st_depths) %in% as.numeric(names(low_bad))])
  }
  if (any(bad_table >= mid)){
    high_bad <- bad_table[bad_table >= mid]
    st_depths[st_depths %in% as.numeric(names(high_bad))] <-
      max(unique(st_depths)[!unique(st_depths) %in% as.numeric(names(high_bad))])
  }

  # Rescale and split by use
  st_depths <- 1 + (st_depths - min(st_depths))
  tr_depths <- st_depths[-(1:nrow(test_df))]
  te_depths <- st_depths[(1:nrow(test_df))]

  # Calcualte FSD of error at each level

  # Create Full depth dt
  depth_df <- data.frame(depth = as.numeric(names(table(st_depths))))

  trdepth_df <- data.frame(depth = as.numeric(names(table(tr_depths))),
                           fsd = tapply(abs(mod_obj$data$error), tr_depths, sd))
  depth_df <- depth_df %>%
    dplyr::left_join(., trdepth_df, by = 'depth')

  if (any(is.na(depth_df$fsd))){
    nas <- which(is.na(depth_df$fsd))
    for (i_na in nas){
      id <- depth_df$depth[i_na]
      nbrs <- abs(id - depth_df$depth)
      xx <- which(!is.na(depth_df$fsd) & nbrs > 0)
      xxp <- xx[which(nbrs[xx] == min(nbrs[xx]))]
      depth_df$fsd[i_na] = median(depth_df$fsd[xxp])
    }
  }

  fsds_ <- as.list(depth_df$fsd[depth_df$depth %in% as.numeric(names(table(te_depths)))])

  # Apply error PIs based on depth level
  tedepth_ <- split(test_df, te_depths)
  purrr::map2(.x = tedepth_,
              .y = fsds_,
              .f = depthInterval,
              intervals = pred_intervals,
              ...) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(iso_depth = te_depths) %>%
    structure(., class = c('dissimilarity', 'interval_df', 'tbl_df', 'tbl', 'data.frame'))
}

#'
#' Calculate the depth-related intervals
#'
#' @param x_df Method to use for interval construction
#' @param fsd Depth specific FSD
#' @param intervals Prediction intervals
#' @importFrom stats qnorm
#' @return Prediction df from depth
#' @export

depthInterval <- function(x_df,
                          fsd,
                          intervals){
  pi_df <- x_df[, c('trans_id', 'price', 'pred', 'error')]

  # Get SD multipliers
  depthintervals_ <- list()
  for (k in 1:length(intervals)) {
    cp <- intervals[[k]]
    depthintervals_[[k]] <- qnorm(.5 + cp / 2) * fsd
  }
  depth_intervals <- unlist(depthintervals_)

  for (k in 1:length(intervals)) {

    lo <- pi_df$pred - (pi_df$pred * depth_intervals[k])
    hi <- pi_df$pred + (pi_df$pred * depth_intervals[k])

    pi_df[[paste0('lo_', format(intervals[k], nsmall = 2))]] <- lo
    pi_df[[paste0('hi_', format(intervals[k], nsmall = 2))]] <- hi
  }
  pi_df
}

#'
#' Interval scoring for model based PIs
#'
#' Generic method for creating prediction intervals from a model based method
#'
#' @param mod_obj Trained model objects
#' @param test_df Data.frame of data to score
#' @param pred_intervals [.05 to .95 by .05] Desired prediction intervals
#' @param ... Additional arguments
#' @return Data.frame of prediction intervals
#' @export

rangePrediction <- function(mod_obj,
                            test_df,
                            pred_intervals,
                            ...){


  UseMethod('rangePrediction', mod_obj)
}

#'
#' Score intervals with linear model
#'
#' @method rangePrediction lm
#' @inherit rangePrediction params
#' @importFrom stats median
#' @importFrom dplyr bind_rows select left_join mutate
#' @export

rangePrediction.lm <- function(mod_obj,
                               test_df,
                               pred_intervals,
                               ...){
  # Set up capture list
  pi_ <- vector('list', length = length(pred_intervals))

  boot_mat <- bootLM(lm_obj = mod_obj$full,
                     new_df = test_df,
                     ...)

  pi_pred <- as.data.frame(t(apply(boot_mat, 1, quantile,
                                   c(sort(.5 - pred_intervals / 2),
                                     .5 + pred_intervals / 2))))

  re_order <- c(length(pred_intervals):1, (1:length(pred_intervals) + .01))
  pi_pred <- pi_pred[, order(re_order)]

  # Fix PI names
  names(pi_pred) <- paste0(rep(c('lo_', 'hi_'), length(pred_intervals)),
                           sort(rep(format(pred_intervals, nsmall = 2), 2)))

  pi_med <- median(pi_pred[[1]], na.rm = TRUE)
  if (is.na(pi_med)) return(NULL)

  test_df %>%
    dplyr::select(trans_id, price, pred, error) %>%
    dplyr::left_join(., pi_pred %>%
                       dplyr::mutate(trans_id = test_df$trans_id), by = 'trans_id')
}


#'
#' @export

adjustResiduals <- function(lm_obj){

  resid_adj <- stats::residuals(lm_obj) /
    sqrt(1.001 - stats::influence(lm_obj, do.coef = FALSE)$hat)
  resid_adj - mean(resid_adj, na.rm = FALSE)
}

#'
#' Core bootstrap routine
#' @param lm_obj Linear model object
#' @param new_df Prediction data.frame
#' @param boot_samples [100] Number of boot strap samples
#' @importFrom caret createFolds
#' @importFrom dplyr bind_rows left_join
#' @return Cross validation prediction
#' @export

bootLM <- function(lm_obj,
                   new_df,
                   boot_samples = 100){

  bs_ <- vector('list', length = boot_samples)
  for (i in 1:boot_samples){
    bs_[[i]] <- bootLMEngine(lm_obj, new_df, seed = i)
    # bs_[[i]] <- bootLMEngine(df, mod_spec, new_df, seed = i)
  }
  unlist(bs_) %>% matrix(., ncol=boot_samples)
}

#'
#' Controls the bootstrapping
#'
#' @param lm_obj Linear model object
#' @param new_df Prediction data
#' @param seed [1] random seed
#' @importFrom caret createFolds
#' @importFrom dplyr bind_rows left_join
#' @return Cross validation prediction
#' @export

bootLMEngine <- function(lm_obj,
                         new_df,
                         seed = 1){

  # Make bootstrap residuals
  set.seed(seed)
  boot_resid <- sample(adjustResiduals(lm_obj),
                       size = length(lm_obj$residuals),
                       replace=TRUE)

  # Make bootstrap Y
  y_boot <- fitted(lm_obj) + boot_resid

  # Do bootstrap regression
  boot_df <- model.frame(lm_obj)
  boot_df[,1] <- y_boot
  if (grepl('log', names(boot_df)[1])){
    names(boot_df)[1] <- 'price'
    boot_df$price <- exp(boot_df$price)
  }
  if (any(grepl('as.factor', names(boot_df)))){
    ix <- grep('as.factor', names(boot_df))
    names(boot_df)[ix] <- 'trans_period'
  }

  lm_boot <- lm(as.formula(lm_obj, env = environment()), data = boot_df)

  # Create bootstrapped adjusted residuals
  boot_resid_adj <- adjustResiduals(lm_boot)

  predict(lm_boot, newdata = new_df) + sample(boot_resid_adj, size = nrow(new_df),
                                              replace = TRUE)
}

#'
#' Score intervals with random forest
#'
#' @method rangePrediction rf
#' @inherit rangePrediction params
#' @importFrom dplyr select left_join mutate
#' @export

rangePrediction.rf <- function(mod_obj,
                               test_df,
                               pred_intervals,
                               ...){

  # Make the quantile Prediction intervals
  pi_pred <- predict(mod_obj$full,
                     data = test_df,
                     type = 'quantiles',
                     quantiles =  c(sort(.5 - pred_intervals / 2),
                                    .5 + pred_intervals / 2))$predictions

  # Re order columns to be in order of intervals and convert to a data.frame
  re_order <- c(length(pred_intervals):1, (1:length(pred_intervals) + .01))
  pi_pred <- as.data.frame(pi_pred[, order(re_order)])

  # Fix PI names
  names(pi_pred) <- paste0(rep(c('lo_', 'hi_'), length(pred_intervals)),
                           sort(rep(format(pred_intervals, nsmall = 2), 2)))

  test_df %>%
    dplyr::select(trans_id, price, pred, error) %>%
    dplyr::left_join(., pi_pred %>%
                       dplyr::mutate(trans_id = test_df$trans_id), by = 'trans_id')
}

# Calibration --------------------------------------------------------------------------------------

#'
#' Calculate calibration for a scored object
#'
#' @param scored_obj An object from scoreModel
#' @importFrom dplyr select left_join mutate
#' @importFrom purrr map
#' @return List of model and error calib and intervals
#' @export

calibrateModel <- function(scored_obj){

  predaccr_df <- calculateAccuracy(scored_obj$score)

  within_ <- purrr::map(.x = scored_obj$predintervals,
                        .f = checkPredIntervals)

  accr_ <- purrr::map(.x = within_,
                      .f = calibEngine)

  effic_ <- purrr::map(.x = scored_obj$predintervals,
                       .f = calibEfficiency,
                       intervals = scored_obj$intervals)

  list(prediction = predaccr_df,
       accr_summ = purrr::map(.x = accr_,
                              .f = function(x) x$summary) %>%
         dplyr::bind_rows() %>%
         dplyr::mutate(method = names(scored_obj$predintervals)),
       accuracy = accr_,
       efficiency = effic_) %>%
    structure(., class = 'calibrationn')
}

#'
#' Calculate calibration for a scored object
#'
#' @param scored_obj An object from scoreModel
#' @importFrom dplyr select left_join mutate
#' @importFrom purrr map
#' @return List of model and error calib and intervals
#' @export

calibEfficiency <- function(interval_df,
                            intervals){

  effic_df <- interval_df %>% dplyr::select(trans_id, price, pred)

  for (k in intervals){
    int_df <- interval_df[,grep(k, names(interval_df))]
    names(int_df) <- c('lo', 'hi')
    int_df$range <- as.numeric(int_df$hi - int_df$lo)
    effic_df[[paste0('range_', k)]] <- int_df$range
  }

  effic_df %>%
    tidyr::gather(key = interval, value = efficiency, -c(trans_id, price, pred)) %>%
    dplyr::group_by(interval) %>%
    dplyr::summarize(mean = mean(efficiency, na.rm=TRUE),
                     median = median(efficiency, na.rm=TRUE)) %>%
    dplyr::mutate(interval = intervals)
}

#'
#' Calculate model accuracy
#'
#' Calculate standard accuracy metrics
#'
#' @param score_df A scored data.frame
#' @importFrom stats median
#' @return Dzta.frame of accuracy stats
#' @export

calculateAccuracy <- function(score_df){

  data.frame(mape = median(abs(score_df$error)),
             mpe = median(score_df$error),
             aape = mean(abs(score_df$error)),
             ape = mean(score_df$error),
             pe10 = sum(abs(score_df$error) <= .10) / nrow(score_df),
             pe20 = sum(abs(score_df$error) <= .20) / nrow(score_df),
             pe30 = sum(abs(score_df$error) <= .30) / nrow(score_df))
}

#'
#' Determine whether or not predictions are within the PI
#'
#' Runs a validation run given an information horizon, produces results
#'
#' @param interval_df A prediction interval data.frame ()
#' @importFrom dplyr select
#' @importFrom tidyselect contains
#' @return Dzta.frame with 0/1 withinness for each PI
#' @export

checkPredIntervals <- function(interval_df){

  if (!'interval_df' %in% class(interval_df)){
    stop('"checkPredIntervals()" requires inputs of class "interval_df"')
  }

  # Extract low and high PI values
  lo_df <- interval_df %>% dplyr::select(tidyselect::contains('lo_'))
  hi_df <- interval_df %>% dplyr::select(tidyselect::contains('hi_'))

  # Check if actual sale price falls above and below ranges
  for (k in 1:ncol(lo_df)){
    lo_df[k] <- ifelse(lo_df[,k] <= log(interval_df$price), 1, 0)
    hi_df[k] <- ifelse(hi_df[,k] >= log(interval_df$price), 1, 0)
  }

  # Determine which are in the range (both > lo and < hi)
  within_df <- Reduce('*', list(lo_df, hi_df))

  # Fix names
  names(within_df) <- gsub('lo', 'within', names(within_df))

  # Return data.frame with calibration yes/nos
  structure(cbind(interval_df %>%
                    dplyr::select(trans_id, price, pred, error),
                  within_df),
            class = c('within', 'data.frame'))
}

#'
#' Calculate calibration for a scored object
#'
#' @param within_df A checkPI completed scored dateaset
#' @importFrom dplyr select left_join mutate
#' @importFrom stats lm median
#' @return List of model and error calib and intervals
#' @export

calibEngine <- function(within_df){


  ## Summary of calibration
  cal_cols <- grep('within_', names(within_df))
  summ_df <- data.frame(interval = as.numeric(gsub('within_', '', names(within_df)[cal_cols])),
                        coverage = unlist(lapply(within_df[, cal_cols], mean, na.rm = TRUE)))

  # MAE
  mae <- median(abs(summ_df$coverage - summ_df$interval))

  # Find shift
  lin_mod <- lm(coverage ~ interval, data = summ_df)

  drift <- as.numeric(lin_mod$coefficients[2] - 1)

  if (abs(summary(stats::lm(coverage ~ interval, data = summ_df))$coefficients[2,3]) < 2.2){
    drift <- 0
    summ_df$res_coverage <- summ_df$coverage
  } else {
    summ_df$res_coverage <- summ_df$coverage / lin_mod$coefficients[2]
  }

  shift <- stats::median(summ_df$res_coverage - summ_df$interval)

  summ_df$res_coverage <- summ_df$res_coverage - shift

  col_seq <- 1:length(cal_cols)
  middle <- which(col_seq >= quantile(col_seq, .25) &
                    col_seq <= quantile(col_seq, .75))
  curve <- (mean(summ_df$res_coverage[middle] - summ_df$interval[middle]) -
              mean(summ_df$res_coverage[-middle] - summ_df$interval[-middle]))

  summ_df$res_coverage <- NULL

  structure(list(data = summ_df,
                 summary = data.frame(MAE = mae,
                                      shift = shift,
                                      drift = drift,
                                      curve = curve)),
            class = 'calibSummary')
}

#############################################################################################
