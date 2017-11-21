[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/antaresViz)](https://cran.r-project.org/package=antaresViz)
[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresViz.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresViz)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/antaresViz?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaresViz)[![codecov](https://codecov.io/gh/rte-antares-rpackage/antaresViz/branch/develop/graph/badge.svg)](https://codecov.io/gh/rte-antares-rpackage/antaresViz)

# The package antaresViz: visualize the results of an Antares simulation

`antaresViz` is the package to visualize the results of your Antares simulations that you have imported in the R session with package `antaresRead`. It provides some functions that generate interactive visualisations. Moreover, by default, these functions launch a shiny widget that provides some controls to dynamically choose what data is displayed in the graphics.

## Installation

This package has been published on CRAN, so you can install it easily:
```r
install.packages("antaresViz")
```

To install the last development version:
```r
devtools::install_github("rte-antares-rpackage/antaresViz", ref ="develop")
```

To display the help of the package and see all the functions it provides, type:
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

You can use `spMaps` to set a map background or download some files at http://www.gadm.org/country.

## Contributing:

Contributions to the library are welcome and can be submitted in the form of pull requests to this repository.

## ANTARES :
 Antares is a powerful software developed by RTE to simulate and study electric power systems (more information about Antares here : <https://antares.rte-france.com>).

## License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com

This Source Code is subject to the terms of the GNU General Public License, version 2 or any higher version. If a copy of the GPL-v2 was not distributed with this file, You can obtain one at https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.
