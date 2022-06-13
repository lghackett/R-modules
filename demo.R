####################################
## Demonstrate use of modules
# Author: Lucy Hackett
# Date: 06/12/2022
####################################
library(pacman)
p_load(fixest, dplyr, box, AER,
       modelsummary, insight)

box::use(outputHelpers/msout, regHelpers/reghelp)

## Load data -----------------------
data("USSeatBelts", package = "AER")

USSeatBelts = USSeatBelts %>% 
  mutate(primary = ifelse(enforce == "primary", 1, 0),
         fatalities_millions = fatalities*1e6)

# double data to illustrate N formatting later
seatbelts = rbind(USSeatBelts, USSeatBelts)

## FEs model -----------------------
# define a set of models
baseX <- c("primary", "miles", "drinkage", "alcohol")
extenX <- c(baseX, "speed65", "speed70", "income")
Xvars <- list(baseX, extenX)

# set names for table columns
colnames <- c("Base controls", "Extended controls")

# estimate models
regs <- lapply(Xvars, function(x){ # loop over X vars
  feols(reghelp$feols_formulator("fatalities_millions", # outcome
                                 x, # iterator var
                                 c("state", "year")), # FEs
        cluster = ~factor(state), # clustering
        data = seatbelts) # data
}) %>% setNames(colnames) # set names

## Create table -----------------------
options(modelsummary_format_numeric_latex = "mathmode")

# add dependent variable mean to stats
# Inspired by Vincent's suggestion on StackOverflow:
# https://stackoverflow.com/questions/71882957/compute-and-display-mean-of-dependent-variable-in-modelsummary-output-tables-in
glance_custom.fixest <- function(x, ...) {
  dv <- insight::find_response(x)
  dat <- insight::get_data(x)
  out <- data.frame("Mean Y" = mean(dat[[dv]]),
                    check.names = FALSE)
  out <-  # format
  return(out)
}

# format statistics from the table
f0 <- function(x) format(round(x, 0), big.mark=",")
f2 <- function(x) format(round(x, 3), big.mark=",")

gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f0),
  list("raw" = "Mean Y", "clean" = "Mean Y", "fmt" = f2)
)

# create table
modelsummary(regs, stars = T,
             coef_rename = c("primary" = "Primary seatbelt laws"),
             coef_omit = "[^primary]",
             fmt = function(x) format(round(x, 2), big.mark=","),
             gof_map = gm)

# LaTeX version of the table, stripped of float environment
(tab = msout$strip_table(
  modelsummary(regs, stars = T, escape=F,
             output = "latex",
             coef_rename = c("primary" = "Primary seatbelt laws"),
             coef_omit = "[^primary]",
             fmt = function(x) format(round(x, 0), big.mark=","),
             gof_map = gm)
))

# saving
# tab %>% kableExtra::save_kable("example.tex")

# Dealing with special chars -----------------------------
# new names just for kicks
newnames <- c("$\\Delta$", "\\%")
regs <- regs %>% setNames(newnames)

msout$strip_table(
  modelsummary(regs, stars = T, escape=F, # escape = F to keep latex
               output = "latex",
               coef_rename = c("primary" = "Primary seatbelt laws"),
               coef_omit = "[^primary]",
               fmt = function(x) format(round(x, 0), big.mark=","),
               gof_map = gm)
)


