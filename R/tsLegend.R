#Copyright © 2016 RTE Réseau de transport d’électricité

#' Plot an interactive legend for time series plots
#' 
#' These functions create a nice looking legend that displays values when the user
#' hovers a time series produced with plot this package. By 
#' default, the different functions already output a legend. This function
#' is mostly useful to share a unique legend between two or more time series plots.
#' 
#' @param labels
#'   vector containing the names of the times series
#' @param colors
#'   vector of colors. for function \code{tsLegend} it must have the same length
#'   as parameter \code{labels}. For function \code{productionStackLegend}, it 
#'   must have same length as parameter \code{variables}. If \code{variables} is
#'   an alias, then this argument should be \code{NULL} in order to use default
#'   colors.
#' @param types
#'   "line" or "area" or a vector with same length as \code{labels} containing 
#'   these two values.
#' @inheritParams productionStack
#' 
#' @details 
#' Thes functions can be used to create a legend shared by multiple plots 
#' in a Shiny application or an interactive document created with Rmarkdown.
#' For instance, let assume one wants to display four productions stacks in a 2x2
#' layout and have a unique legend below them in a Rmarkdown document. To do so,
#' one can use the following chunck code:
#' 
#' \preformatted{
#' ```{R, echo = FALSE}
#' library(shiny)
#' 
#' fillCol(height = "600px", flex = c(1, 1, NA),
#'   fillRow(
#'     productionStack(mydata, areas = "fr", 
#'                     main = "Production stack in France", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'     productionStack(mydata, areas = "de", 
#'                     main = "Production stack in Germany", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   ),
#'   fillRow(
#'     productionStack(mydata, areas = "es", 
#'                     main = "Production stack in Spain", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'     productionStack(mydata, areas = "be", 
#'                     main = "Production stack in Belgium", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   ),
#'   productionStackLegend(legendId = 1)
#' )
#' ```
#' }
#' 
#' @export
tsLegend <- function(labels, colors, types = "line", legendItemsPerRow = 5, legendId = "") {
  
  legendItems <- mapply(.tsLegendItem, 
                        label = labels, 
                        color = colors,
                        type = types,
                        legendId = legendId,
                        SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
  
  
  nbRows <- ceiling(length(legendItems) / legendItemsPerRow) 
  
  legendItems <- rev(legendItems)
  
  legendRows <- list()
  for (i in 1:nbRows) {
    j <- ((i - 1) * legendItemsPerRow + 1):(i * legendItemsPerRow)
    legendRows[[i]] <- do.call(fillRow, legendItems[j])
  } 
  
  legendItems <- tags$div(
    fillRow(do.call(fillCol, legendRows), height = i * 20),
    height = i * 20,
    style = "padding-left: 100px;"
  )
  
  tags$div(
    style="position:relative;",
    height = max(i * 20, 40),
    tags$div(
      style = "position:absolute;top:0;bottom:0;width:100px;
",
      tags$div(
        style="text-align:center;font-weight:bold;font-size:16px",
        id = paste0("date", legendId)
      )
    ),
    legendItems
  )
}

.style <- function(...) {
  values <- list(...)
  keys <- names(values)
  
  keyval <- paste(keys, values, sep = ":")
  paste(keyval, collapse = ";")
}

.tsLegendItem <- function(label, color, legendId, type = "line") {
  h <- "16px"
  
  if (type == "area") {
    
    c <- col2rgb(color)
    o <- (c[1] * 299 + c[2] * 587 + c[3] * 114) / 1000
    txtCol <- ifelse(o > 170, "black", "white")
    
    content <- tags$div(
      style = .style(color = txtCol, "background-color" = color, 
                     position= "absolute", left = "2px", right = "4px", height=h),
      tags$div(
        style = .style(position="absolute", left="4px", padding="0 2px"), 
        class = "leglabel",
        label),
      tags$div(
        style= .style(position="absolute", right="0px", padding="0 2px 0 4px",
                      "background-color"=color, "font-weight"="bold",
                      height="16px"), 
        id = paste0(label, legendId), 
        class =  "legvalue",
        ""
      )
    )
    
  } else if (type == "line") {
    
    content <- tags$div(
      style = .style(color = color, 
                     width = "100%", height=h),
      tags$div(
        style = .style(position="absolute", "background-color"=color,
                       left="4px", right="4px", top ="8px", bottom="8px"), 
        ""),
      tags$div(
        style = .style(position="absolute", left="4px", padding="0 4px 0 2px",
                       "background-color"="white"), 
        class = "leglabel",
        label),
      tags$div(
        style= .style(position="absolute", right="4px", padding="0 2px 0 4px",
                      "background-color"="white", "font-weight"="bold",
                      height="16px"), 
        id = paste0(label, legendId), 
        class =  "legvalue",
        ""
      )
    )
    
  }
  
  tags$div(
    style = .style(width = "100%", height = h, padding = "0 4px", "font-size"= "12px"),
    content
  )
}

JS_addLegend <- JS('
function(el, x, data) {
  var htmlwidgetContainer = el.parentElement; 
  
  var container = document.createElement("div");
  container.id = "container";
  container.style.height = el.style.height;
  container.style.width = el.style.width;
  container.style.position = "relative";
  
  el.style.position = "absolute";
  el.style.top = "0";
  el.style.bottom = data.size + "px";
  el.style.height = "auto";
  container.appendChild(el);
  
  var leg = document.createElement("div");
  leg.style.height = data.size + "px";
  leg.style.position = "absolute";
  leg.style.bottom = "0";
  leg.style.left = "0";
  leg.style.right = "0";
  leg.innerHTML = data.html;
  container.appendChild(leg);
  
  htmlwidgetContainer.appendChild(container);
  
  this.resize();
}
')

JS_updateLegend <- function(legendId, timeStep = "hourly") {
  
  # Function that transform a timestamp ta a date label
  timeToLab <- switch(
    timeStep,
    hourly = "var date = new Date(x); 
              var day = date.toString().slice(0, 10);
              var h = date.toString().slice(16, 21);
              return day + '<br/>' + h;",
    daily = "var date = new Date(x); return date.toString().slice(0, 10)",
    weekly = "var date = new Date(x); return date.toString().slice(0, 10)",
    monthly = "var date = new Date(x); return date.toString().slice(4, 8)",
    "return x"
  )
  
  script <-"
function(e, timestamp, data) {
  function timeToLab(x) {%s}
  document.getElementById('date%s').innerHTML = timeToLab(timestamp);
  
  var values = {};

  data.forEach(function(d) {
    var sign = d.name.match(/^neg/) ? -1 : 1;
    var varname = d.name.replace(/^neg/, '');
    if (values[varname]) values[varname] += d.yval * sign;
    else values[varname] = d.yval * sign;
  })

  for (k in values) {
    if (!values.hasOwnProperty(k)) continue; 
    var el = document.getElementById(k + '%s');
    if (el) el.innerHTML = Math.round(values[k]);
  }
}"
  
  JS(sprintf(script, timeToLab, legendId, legendId))
}


JS_resetLegend <- function(legendId) {
  script <- "
function(e) {
  document.getElementById('date%s').innerHTML = '';
  var els = document.getElementsByClassName('legvalue');
  for (var i = 0; i < els.length; ++i) {els[i].innerHTML = '';}
}"
  
  JS(sprintf(script, legendId))
}
