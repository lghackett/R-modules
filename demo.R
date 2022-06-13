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
        data = USSeatBelts) # data
}) %>% setNames(colnames) # set names

## Create table -----------------------
options(modelsummary_format_numeric_latex = "mathmode")

# add dependent variable mean to stats
glance_custom.fixest <- function(x, ...) {
  dv <- insight::find_response(x)
  dat <- insight::get_data(x)
  out <- data.frame("Mean Y" = mean(dat[[dv]]), check.names = FALSE)
  out <- format(round(out, 2), big.mark=",") # format
  return(out)
}

# format statistics from the table
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "Mean Y",    "Mean Y",        2
)

# create table
modelsummary(regs, stars = T,
             coef_rename = c("primary" = "Primary seatbelt laws"),
             coef_omit = "[^primary]",
             fmt = function(x) format(round(x, 2), big.mark=","),
             gof_map = gm)

# LaTeX version of the table, stripped of float environment
msout$strip_table(
  modelsummary(regs, stars = T,
             output = "latex",
             coef_rename = c("primary" = "Primary seatbelt laws"),
             coef_omit = "[^primary]",
             fmt = function(x) format(round(x, 0), big.mark=","),
             gof_map = gm)
)


