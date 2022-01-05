#' Regression model helpers
#'
#' The \code{regHelpers/reghelp} module provides helper functions for 
#' estimating econometric models in R.
'.__module__.'

#' FEOLS formulator
#' 
#' Write formula for feols::feols() using a string for the outcome, a vector of
#' strings for the X variables to facilitate iterating over output 
#' variables in feols(). 
#' @param outcome string. Yvariable in the regression
#' @param xvec vector of strings. Xvariables (excluding FE) in the regression
#' @param fe vector of strings, FE's to be added.
#' @return A \code{formula} appropriate for feols
#' @export
feols_formulator <- function(outcome, xvec, fe){
  formula <- outcome
  formula <- paste(formula, '~', 
                   paste(xvec, collapse = ' + '), '|',
                   paste(fe, collapse = ' + '))
  return(stats::as.formula(formula))
}