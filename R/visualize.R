#***************************************************************************************************
#
#  Wrapper functions for supporting uncertainty/calibration research
#
#***************************************************************************************************

#'
#' Validation of a training model/data
#'
#' Runs a validation run given an information horizon, produces results
#'
#' @param cal_obj object from calibrateModel()
#' @param title_prefix ['Calibration'] Title prefix
#' @param xaxis_gap [.1] Gap for x axis
#' @param yaxis_gap [.1] Gap for y axis
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows mutate
#' @importFrom ggplot2 ggplot
#' @return plot of calibration comparisons
#' @export

plotCalib <- function(cal_obj,
                      title_prefix = 'Calibration',
                      xaxis_gap = .1,
                      yaxis_gap = .1){

  require(ggplot2)

  if ('uncertainty' %in% class(cal_obj)) cal_obj <- cal_obj$calib

  x_df <- purrr::map2(.x = cal_obj$accuracy,
                      .y = names(cal_obj$accuracy),
                      .f = function(x,y){
                        x$data %>% dplyr::mutate(method = y)
                      }) %>%
    dplyr::bind_rows()

  ggplot(x_df, aes(x = interval, y = coverage, color = method)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, size = 1, linetype = 1, color = 'gray50') +
    scale_y_continuous(breaks = seq(.05, .95, yaxis_gap)) +
    scale_x_continuous(breaks = seq(.05, .95, xaxis_gap)) +
    scale_color_manual(name = '', values = c('red', 'orange', 'blue', 'black')) +
    xlab('\nConfidence Level') +
    ylab('PI Coverage\n') +
    ggtitle(title_prefix) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom') +
    geom_line(data = x_df, aes(x=interval, y = coverage, color = method),
              size = 1.2) +
    geom_text(x=.2, y=.8, label = 'Too Conservative', color = 'gray60',
              size = 6) +
    geom_text(x=.8, y=.2, label = 'Too Aggressive', color = 'gray60',
              size = 6)

}

#'
#' Validation of a training model/data
#'
#' Runs a validation run given an information horizon, produces results
#'
#' @param cal_obj object from calibrateModel()
#' @param title_prefix ['Calibration'] Title prefix
#' @param xaxis_gap [.1] Gap for x axis
#' @param yaxis_gap [.1] Gap for y axis
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows mutate
#' @importFrom ggplot2 ggplot
#' @return plot of calibration comparisons
#' @export

plotEffic <- function(cal_obj,
                      title_prefix = 'Interval Efficiency',
                      xaxis_gap = .1,
                      yaxis_gap = .1){

  require(ggplot2)

  if ('uncertainty' %in% class(cal_obj)) cal_obj <- cal_obj$calib

  x_df <- purrr::map2(.x = cal_obj$efficiency,
                      .y = names(cal_obj$efficiency),
                      .f = function(x, y){
                        x %>% dplyr::mutate(method = y)
                      }) %>%
    dplyr::bind_rows()

  ggplot(x_df, aes(x = interval, y = median, color = method)) +
    geom_point() +
    scale_y_continuous(breaks = seq(.05, .95, yaxis_gap)) +
    scale_x_continuous(breaks = seq(.05, .95, xaxis_gap)) +
    scale_color_manual(name = '', values = c('red', 'orange', 'blue', 'black')) +
    xlab('\nPrediction Interval') +
    ylab('Median Interval Width\n') +
    ggtitle(title_prefix) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom') +
    geom_line(data = x_df, aes(x=interval, y = median, color = method),
              size = .8)

}

#'
#' Plot pi quantiles vs errors cumulatives
#'
#' @param score_obj object from calibrateModel()
#' @param plot_title ['Insert Title Here'] Title
#' @param pi_width [50] PI width to examine in the Title prefix
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot
#' @return plot of error vs pi quantiles
#' @export

plotQuantiles <- function(score_obj,
                          plot_title = 'Insert Title Here',
                          pi_width = 50){

  if ('uncertainty' %in% class(score_obj)) score_obj <- score_obj$score

  plot_df <- purrr::map2(.x = score_obj$predintervals,
                         .y = names(score_obj$predintervals),
                         .f = extractQuantiles,
                         pi_width = pi_width) %>%
    dplyr::bind_rows()

  plot_df <- plot_df %>%
    dplyr::bind_rows(., plot_df %>%
                       dplyr::filter(method == 'model') %>%
                       dplyr::mutate(method = 'perfect',
                                     pi = error))

  ggplot(plot_df,
         aes(x = quantile, y = pi, color = method, group = method)) +
    geom_line(size = .8) +
    ggtitle(plot_title) +
    scale_color_manual(name = '', values = c('red', 'orange', 'blue', 'black')) +
    ylab('MAPE\n') +
    xlab('Cumulative Quantile') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom')

}

#############################################################################################
