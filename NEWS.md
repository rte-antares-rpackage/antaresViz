Copyright 2016 RTE Reseau de transport d'electricite

# antaresViz 0.18.1

## new feature : 
* `mapLayout_no_interactive()` make an object of class "mapLayout" from external geojson file

# antaresViz 0.18.0
* fix deprecated dependencies (issue #200)
  * packages `rgeos`, `raster` removed and replaced by `sf`

# antaresViz 0.17.1

* Fixed tests failing on CRAN.

# antaresViz 0.17

* antares V8.1 support

# antaresViz 0.16

* Fixed bug with new shiny version
* Fixed bug due to language
* manipulateWidget > 0.11 support

# antaresViz 0.15.4

* Fixed tests failing on CRAN

# antaresViz 0.15.3 (2020-11-12)

* Fixed tests failing on CRAN.


# antaresViz 0.15.2 (2020-05-26)

* Fixed CRAN errors. Replaced {rbokeh} dependency by {plotly} + {ggplot2}.


# antaresViz 0.15.0 (2018-09-28)

NEW FEATURES:
* In prodStack(), exchangesStack(), tsPlot() and plotMap() it is now possible to compare several studies with a reference. A new  'refStudy' parameter can be used to set the reference study. 
* runAppAntaresViz() we can use a reference study with all manipulateWidget modules.

BUGFIXES:
* prodStack() and exchangesStack() were not working correctly with a list of antaresData or antaresDataList, only one legend was working when the interactive parameter was set to FALSE.
* plotMap() was not working correctly with a list of optsH5, the first graph was not updated correctly when the interactive parameter was set to TRUE.


# antaresViz 0.14.0 (2018-06-12)

NEW FEATURES:
* In prodStack(), exchangesStack() and tsPlot(), it is now possible to save a plot as PNG or HTML.


# antaresViz 0.13.0 (2018-05-03)

NEW FEATURES:
* In runAppAntaresViz(), prodStack(), exchangesStack() and tsPlot(), it is now possible to change the language from 'en' to 'fr'.
* prodStack() and exchangesStack() can now represent annual data. 
* In plotMap(), it is now possible to give an alias to the parameter "size". A user can specify a new alias with the function setAlias() of 'antaresRead'.
* tsPlot() can now plot several mcYear for several variables. 

BUGFIXES:
* plotMap() was not working with 'leaflet' version 2.0.0. 


# antaresViz 0.12.0 (2017-12-14)

NEW FEATURES:
* plot(), prodStack(), exchangesStack(), plotMap() work with studies in h5. 
* new function runAppAntaresViz(). 
* new function plotThermalGroupCapacities(). 
* new function limitSizeGraph(). 
* new function plotXY(). 
* new function modXY(). 
* new function stackMap(). 
* new option stepPlot in plot(), prodStack() and exchangesStack(). 


# antaresViz 0.11 (2017-07-17)

NEW FEATURES:
* In plotMap(), it is now possible to represent areas with polygons instead of circles. To do see, user needs to provide a SpatialPolygonsDataFrame object to function mapLayout(). He is then able to interactively set associations between an area and a polygon. 

BUGFIXES:
* Hour was not correctly printed in some charts.
* It was not possible anymore to choose a specific Monte-Carlo scenario in plotMap() when interactive = FALSE.
* Non numeric columns were not correcly handled by plotMap().
* Heatmaps were not correctly displayed in comparison mode.


# antaresViz 0.10 (2017-06-20)

BREAKING CHANGES:
* Function plotMapOptions() has lost some parameters of little use and has gained new ones. This may break some scripts.

NEW FEATURES:
* Comparison mode: All functions can now be used to compare two or more antaresData objects. It is also possible to use a single antaresData object but to compare multiple charts with different parameters.

BUGFIXES:
* mapLayout() was containing references to the deprecated 'mapStudio' package. This causes a crash on computers where this package has not been installed when it was alive.  


# antaresViz 0.9 (2017-05-31)

NEW FEATURES:
* plotMap() can now export animated maps. 
* It is now possible to set minimal and maximal size of labels in maps.
* plotMap() can now bind non-numeric columns to the color of areas and links. 
* It is now possible to set the size of plots in Rmarkdown documents with "runtime:shiny"

BUGFIXES:
* plotMap(interactive=TRUE) now works in Rmarkdown documents with "runtime:shiny"


# antaresViz 0.8 (2017-04-07)

NEW FEATURES:
* All visualisation functions now accept synthetic and detailed data. A new parameter "mcYear" permits to choose whether to view averaged data or a given Monte-Carlo scenario.
* plotMap() now always uses the absolute value of variable 'FLOW LIN.' to avoid any confusion. Direction of flows are represented with arrows.
* plotMap() can now also represent categorical variables.
* It is now possible to use custom color palette for continuous color scales in maps and heatmaps.
* In heatmaps, the y-axis now contains month names instead of week ids. Moreover the info displayed when hovering data has been improved.
* exchangesStack() can now also represent exchanges with rest of the world (column 'ROW BAL.').
* A new function called setInteractivity() has been added to globally turn off or turn on interactive mode.

BUGFIXES:
* In plotMap(), several parameters were not working in non-interactive mode.
* In plotMap(), some links were not displayed if the associated values were small.
* Numbers are now correctly rounded on popups and legend in plotMap().
* Invalid date/hours were displayed on charts due to problems of timezone.
* It was not possible to change width and height of heatmaps.


# antaresViz 0.7 (2017-02-23)

NEW FEATURES:
* plot() method can now generate heatmaps.
* plot() method has a new parameter "aggregate" to choose whether to visualize individual plots or aggregated plot.
* plotMap() can now display labels. This may require to increase area size so there is enough place for labels.

BUGFIXES:
* Many small problems detected by R CMD CHECK have been solved.


# antaresViz 0.6 (2017-01-23)

BREAKING CHANGES:
* productionStack and has been renamed prodStack.

NEW FEATURES:
* plotMap() can now use pie charts, bar charts and polar area charts to represent multiple variables.
* plotMap() can now represent average values on the whole period instead of representing only a single time step.
* plotMap() can now display labels on areas. This feature is still experimental for now and will be improved in next versions.
* It is now possible to choose which variables are displayed in popups in maps created with plotMap().
* It is now possible to manually set break points for color scales in maps thourgh parameter "options" of plotMap.
* plot() method for antaresDataTable can now output monotone, density and cululated distribution. 
* There is now a plot() method for antaresDataList objects.
* It is now possible to register custom production stack aliases with function 'setProdStackLegend'. New aliases are then available in prodStack().
* The package now also provides a plot method for antaresDataList objects.

BUGFIXES:
* plot() was not working with annual data.


# antaresViz 0.5 (2016-11-14)

NEW FEATURES:
* plotMap() can now represent only areas and/or links. It is not necessary anymore to import both to create a map.
* plotMap() can now represent multiple variables with barcharts. A new option is available to choose whether to use the same scale for different variables or one scale per variable.
* plotMap() now displays a nice and clear legend. 
* It is now possible to add a title to a map.
* plotMap() has a new parameter "options". With this parameter, user can modify many graphical parameters like colors, size of elements, parameters to construct color scales, etc. 
* It is now possible to use a custom base map imported with package "sp". So an internet connection is not necesary anymore to display a nice map.
* When user clicks on a link or an area in a map, a popup appears and display values of the variables represented.


# antaresViz 0.4 (2016-10-06)

BREAKING CHANGES:
* add a file LICENSE and copyright to sources files


# antaresViz 0.3 (2016-09-27)

NEW FEATURES:
* New function exchangesStack to draw the exchanges of an area with its neighbours
* New function mapLayout that helps the user to bind areas of an antares study with geographical coordinates. The function launches an interactive widget where the user can place and move areas.
* New plot method for objects return by mapLayout(). It generates an interactive map that represents the network of an antares study.
* New function plotMap to visualize the results of an Antares simulation on a map.
* All functions have gained new arguments to control graphical parameters like title, axis label, colors, width and height.
* All functions that produce time series (stacked or not) now produce a beautiful and very clear legend.
* Time series plots (stacked or not) can share a unique legend. When they do, their zoom is synchronised: if the users zooms on one graphic, the zoom is also applied to the other graphics. This can be very helpful in shiny application or interactive documents produced with Rmarkdown.


# antaresViz 0.2 (2016-08-22)

NEW FEATURES:
* plot method for antaresTable objects and productionStack have an improved interface that helps user choose what he wants to visualize.
* plot method for antaresTable objects can now draw time series but also barplots and monotones.
