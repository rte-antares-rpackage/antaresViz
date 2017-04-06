[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresViz.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresViz)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/antaresViz?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaresViz)

# The package antaresViz: visualize the results of an Antares simulation

`antaresViz` is the package to visualize the results of your Antares simulations that you have imported in the R session with package `antaresRead`. It provides some functions that generate interactive visualisations. Moreover, by default, these functions launch a shiny widget that provides some controls to dynamically choose what data is displayed in the graphics.

## Installation

To install the package from Github, you will need to create a personal access token (PAT) here: https://github.com/settings/tokens .

```r
# Install dependencies
install.packages(
  c("data.table", "plyr", "lubridate", "dygraphs", "shiny", "miniUI", "magrittr",
    "highcharter", "tibble", "htmltools", "htmlwidgets", "manipulatewidget", 
    "mapStudio", "leaflet", "sp", "devtools")
)

# Install github dependencies
myToken <- "your_github_PAT"

install_github("rte-antares-rpackage/antares-rpackageRead", auth_token = myToken)
install_github("rte-antares-rpackage/antares-rpackageProcessing", auth_token = myToken)
install_github("rte-antares-rpackage/mapStudio", auth_token = myToken)
install_github("rte-antares-rpackage/manipulateWidget", auth_token = myToken)

# Install the last release of antaresViz
install_github("rte-antares-rpackage/antares-rpackageViz", auth_token = myToken)
```

If you are behind a proxy, you first need to run the following commands:

```r
library(httr)
set_config(use_proxy("XXX.XXX.XX.XX", port=XXXX, username="proxy_user", password="passwd"))
```

To install the last development version:
```r
install_github("rte-antares-rpackage/antares-rpackageViz", 
               auth_token = myToken, ref ="develop")
```

To display the help of the package and see all the functions it provides, type:
```r 
help'(package="antaresViz")
```

# Basic plots

`antaresViz` provides a plot method for tables generated with `antaresRead`. This method is for visualizing a single variable in different formats (times series, barplot, monotone, distribution and cumulative distribution). For instance, the following code displays the distribution of marginal price in different areas.

```
mydata <- readAntares(areas = "all")
plot(mydata, variable = "MRG. PRICE")
```

For more information, run:

```r
?plot.antaresDataTable
```

# Stacks

Function `prodStack` generates a production stack for a set of areas. Different stacks have been defined. One can see their definition with command `productionStackAliases()`.

With function `exchangesStack`, one can visualize the evolution and origin/destination of imports and exports for a given area.

# Maps

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

##Contributing:

Contributions to the library are welcome and can be submitted in the form of pull requests to this repository.

##License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com

This Source Code is subject to the terms of the GNU General Public License, version 2 or any higher version. If a copy of the GPL-v2 was not distributed with this file, You can obtain one at https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.
