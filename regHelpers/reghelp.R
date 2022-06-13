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
#' @return A \code{formula} appropriate for feols. This will look like:\cr\cr
#' outcome ~ xvec | fe
#' @export
feols_formulator <- function(outcome, xvec, fe=NULL){
  formula <- outcome
  if(is.null(fe)){
    formula <- paste(formula, '~', 
                     paste(xvec, collapse = ' + '))  
  }else{
    formula <- paste(formula, '~', 
                     paste(xvec, collapse = ' + '), '|',
                     paste(fe, collapse = ' + '))
  }
  return(stats::as.formula(formula))
}

#' Event study formulator
#' 
#' Write formula for feols::feols() using a string for the outcome, a vector of
#' strings for the X variables to facilitate iterating over output 
#' variables in feols(), formatted in a way that iplot understands.
#' @param outcome string. Yvariable in the regression
#' @param eventvars vector of strings, event study FE's to be added. In the case of a 
#' regular event study, these are usually event time and the treatment variable.
#' In the case of the Sun and Abrahan (2020) estimator, these are the year of 
#' treatment and the year variable.
#' @param xvec vector of strings. Xvariables (excluding FE) in the regression
#' @param fe The fixed effects to be added to the study. Usually include ID and 
#' year FEs. 
#' @param ref = c(-1): Reference years. May include more than 1 if there is a 
#' never treated group.
#' @param sunab = FALSE: A boolean for whether the Sun and Abrahan (2020) estimator
#' should be calculated instead of a traditional event study.
#' @return A \code{formula} appropriate for feols. For a traditional event study,
#' This will look like:\cr\cr
#' outcome ~ i(eventvars, ref = ref) + xvec | fe \cr\cr
#' For Sun and Abraham (2020): \cr\cr
#' outcome ~ sunab(eventvars) + xvec | fe
#' @export
eventstudy_formulator <- function(outcome, eventvars, xvec, fe, ref=c(-1), sunab=F){
  formula <- outcome
  
  if(is.null(xvec)){
    x_string <- ""
  }else{
    x_string <- paste(' + ', xvec, collapse = ' + ')
  }
  
  if(sunab){
    formula <- paste(formula, ' ~ sunab(',
                     paste(eventvars, collapse = ', '), ')',
                     x_string, '|',
                     paste(fe, collapse = ' + '))    
  }else{
    formula <- paste(formula, '~ i(',
                     paste(eventvars, collapse = ' , '), ', ref = c(',
                     paste(ref, collapse = ' , '), '))',
                     x_string, '|',
                     paste(fe, collapse = ' + '))   
  }
  return(stats::as.formula(formula))
}