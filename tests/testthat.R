library(testthat)
library(antaresViz)

# CRAN limite CPU usage
  # issue : 
data.table::setDTthreads(2)

test_check("antaresViz")
