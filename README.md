# R-modules
Collection of modules for use in R.

These modules are built using and require the ``box`` package in R. Viewing the module documentations additionally require the ``roxygen2`` package. 

For a demonstration of some of the funcionality, see ``demo.R``. 

## Warnings
This code is very much a work in progress, and is intended for personal use!! As such, it has not been tested on every OS or for every use case. I welcome feedback and improvements, but if you are interested in using it the safest way is to use it as inspiration for your own, locally developed and maintained code.

## Contents
* The ``outputHelpers`` directory contains modules that provide helper functions to creating tables and figures. 
	- ``msout.R``: Helper functions for creating regression tables with ``modelsummary()``. 
* The ``regHelpers`` directory contains modules that provide helper functions to estimating econometric models. These include wrapper functions for iterating over models.
	- ``reghelp.R``: Helper functions for estimating regressions. For example, includes a helper for ``feols::feols()`` that creates formulas from strings that facilitates iterating over outome variables. 

## Use
To use a module in an R script, you must first set the ``box`` search path with ``options(box.path = 'boxdirectory')``. 
Then, load the module with ``box::use(modulesubpath)``. 
To view the documentation for a function contained in a module, use ``box::help(modulename$functionname)``. 

### Example
```
#install.packages("box")
#install.packages("roxygen2")

options(box.path = '/users/hackettl/documents/github/r-modules/')
box::use(outputHelpers/msout)

box::help(msout$add_rows)
```
