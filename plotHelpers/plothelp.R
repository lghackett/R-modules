#' Plot helpers
#'
#' The \code{plotHelpers/plothelp} module provides helper functions for 
#' drawing graphs in R.
'.__module__.'

#' Event Study (feols)
#' 
#' Plot a simple event study for the output of an feols estimate.
#' @param model feols regression model object.
#' @param xlab  string for xaxis label.
#' @param ylab  string for yaxis label. When unspecified, gives 
#' "Estimate and (1-alpha)*100 \% CI"
#' @param title string for plot title. 
#' @param alpha float. Confidence interval alpha.
#' @param vline boolean for including a vertical line
#' @param vline_loc where to put vertical line (x intercept)
#' @return A plot object
#' @export
draw_event_graph <- function(model, xlab='Event time', ylab=NULL, title='', alpha=0.05, vline=F, vline_loc=-1){
  # tidy the model for extracting statistics
  model <- broom::tidy(summary(model))
  # get rid of any control variable coefficients
  model <- dplyr::filter(model, stringr::str_detect(term, "::"))
  # get event time as its own numeric column
  model <- tidyr::separate(model, 
                           term, 
                           sep = "::", 
                           into = c("var", 'eventtime'))
  model <- dplyr::mutate(model, eventtime = as.numeric(eventtime))
  # fill in any uncalculated coefficients
  eventtime <- min(model$eventtime):max(model$eventtime)
  range <- tibble::tibble(eventtime)
  model <- dplyr::right_join(model, range, by='eventtime')
  # calculate confidence intervals
  model <- dplyr::mutate(model, 
                         estimate = ifelse(is.na(estimate), 0, estimate),
                         std.error = ifelse(is.na(std.error), 0, std.error),
                         upper_ci = estimate + std.error*stats::qnorm(1-alpha/2),
                         lower_ci = estimate - std.error*stats::qnorm(1-alpha/2))
  # format the default ylab
  if(is.null(ylab)){
    ylab <- paste0("Estimate and ", format((1-alpha)*100, digits=0), "% CI")
  }
# plot  
 plot <- ggplot2::ggplot(model, ggplot2::aes(eventtime, estimate)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_errorbar(ggplot2::aes(ymax=upper_ci, ymin=lower_ci)) + 
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = 'grey20') + 
  ggplot2::labs(x = xlab, y = ylab, title = title) +  
  ggplot2::scale_y_continuous(labels = scales::comma) + 
  ggplot2::theme_bw() 
  
 if(vline){
   plot <- plot + ggplot2::geom_vline(xintercept = vline_loc, 
                                      linetype = 2, color = 'grey30')
 }
  return(plot)
}


