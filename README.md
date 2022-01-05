# R-modules
Collection of modules for use in R.

These modules are built using and require the ``box`` package in R. Viewing the module documentations additionally require the ``roxygen2`` package. 

## Use
To use a module in an R script, you must first set the ``box`` search path with ``options(box.path = 'boxdirectory')``. 
Then, load the module with ``box::use(modulesubpath)``. 
To view the documentation for a function contained in a module, use ``box::help(modulename$functionname)``. 

### Example
```
options(box.path = '/users/hackettl/documents/github/r-modules/')
box::use(outputHelpers/msout)
```