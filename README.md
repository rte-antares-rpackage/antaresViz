<img src="man/figures/antares_simulator.png" align="right" width=250 />
<br/>

# antaresViz 

> `antaresViz` is the package to visualize the results of your Antares simulations that you have imported in the R session with package `antaresRead`. It provides some functions that generate interactive visualisations. Moreover, by default, these functions launch a shiny widget that provides some controls to dynamically choose what data is displayed in the graphics.

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/antaresViz)](https://cran.r-project.org/package=antaresViz)
[![codecov](https://codecov.io/gh/rte-antares-rpackage/antaresViz/branch/develop/graph/badge.svg)](https://codecov.io/gh/rte-antares-rpackage/antaresViz)
[![R build status](https://github.com/rte-antares-rpackage/antaresViz/workflows/R-CMD-check/badge.svg)](https://github.com/rte-antares-rpackage/antaresViz/actions)
<!-- badges: end -->


## Installation

You can install stable version from CRAN with:

```r
install.packages("antaresViz")
```

To install the last development version:

```r
devtools::install_github("rte-antares-rpackage/antaresViz")
```

To display the help of the package and see all the functions it provides, use:

```r 
help(package="antaresViz")
```


## Basic plots

`antaresViz` provides a plot method for tables generated with `antaresRead`. This method is for visualizing a single variable in different formats (times series, barplot, monotone, distribution and cumulative distribution). For instance, the following code displays the distribution of marginal price in different areas.

```
mydata <- readAntares(areas = "all")
plot(mydata, variable = "MRG. PRICE")
```

For more information, run:

```r
?plot.antaresDataTable
```


## Stacks

Function `prodStack` generates a production stack for a set of areas. Different stacks have been defined. One can see their definition with command `productionStackAliases()`.

With function `exchangesStack`, one can visualize the evolution and origin/destination of imports and exports for a given area.



## Maps

The construction of maps first requires to associate geographic coordinates to the areas of a study. antaresViz provides function `mapLayout` to do interactively this association.

```r
# Get the coordinates of the areas as they have been placed in the antaresSoftware
layout <- readLayout()

# Associate geographical coordinates
myMapLayout <- mapLayout(layout)

# This mapping should be done once and the result be saved on disk.
save(myMapLayout, file = "myMapLayout.rda")

```

Then map can be generated with function `plotMap`:

```r
myData <- readAntares(areas = "all", links = "all")
plotMap(myData, myMapLayout)
```

You can use `spMaps` to set a map background or download some files at https://gadm.org/download_country_v3.html.


## Contributing:

Contributions to the library are welcome and can be submitted in the form of pull requests to this repository.

## ANTARES :
 Antares is a powerful software developed by RTE to simulate and study electric power systems (more information about Antares here : <https://antares-simulator.org/>).
 
 ANTARES is now an open-source project (since 2018), you can download the sources [here](https://github.com/AntaresSimulatorTeam/Antares_Simulator) if you want to use this package. 


## License Information:

Copyright 2015-2016 RTE (France)

* RTE: https://www.rte-france.com/

This Source Code is subject to the terms of the GNU General Public License, version 2 or any higher version. If a copy of the GPL-v2 was not distributed with this file, You can obtain one at https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.
