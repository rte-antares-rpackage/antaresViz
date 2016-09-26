#' Plot an interactive legend for a production stack plot
#' 
#' This function create an nice looking legend that displays values when the user
#' hovers a production stack created with \code{\link{productionStack}}. By 
#' default \code{\link{productionStack}} already outputs a legend. This function
#' is mostly usefull to share legend between two or more production stacks.
#' 
#' @inheritParams productionStack
#' 
#' @details 
#' This function can be used to create a legend shared by multiple production 
#' stacks in a Shiny application or an interactive document created with Rmarkdown.
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
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'     productionStack(mydata, areas = "de", 
#'                     main = "Production stack in Germany", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'   ),
#'   fillRow(
#'     productionStack(mydata, areas = "es", 
#'                     main = "Production stack in Spain", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'     productionStack(mydata, areas = "be", 
#'                     main = "Production stack in Belgium", unit = "GWh", 
#'                     legend = FALSE, legendId = 1, height = "100\%"),
#'   ),
#'   productionStackLegend(legendId = 1)
#' )
#' ```
#' }
#' 
#' 
#' 
#' @export
tsLegend <- function(labels, colors, types = "line", itemsByRow = 5, legendId = "") {
  
  legendItems <- mapply(.tsLegendItem, 
                        label = labels, 
                        color = colors,
                        type = types,
                        legendId = legendId,
                        SIMPLIFY = FALSE, 
                        USE.NAMES = FALSE)
  
  legendItems <- append(legendItems, list(.tsLegendItem(type = "date", legendId = legendId)))
  
  nbRows <- ceiling(length(legendItems) / itemsByRow) 
  
  legendItems <- rev(legendItems)
  
  legendRows <- list()
  for (i in 1:nbRows) {
    j <- ((i - 1) * itemsByRow + 1):(i * itemsByRow)
    legendRows[[i]] <- do.call(fillRow, legendItems[j])
  } 
  
  tags$div(fillRow(do.call(fillCol, legendRows), height = i * 20), height = i * 20)
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
                     width = "100%", height=h),
      tags$div(
        style = .style(position="absolute", left="4px", padding="0 2px"), 
        class = "leglabel",
        label),
      tags$div(
        style= .style(position="absolute", right="4px", padding="0 2px 0 4px",
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
    
  } else if (type == "date") {
    content <- tags$div(
      style = "text-align:center;",
      tags$span(id = paste0("date", legendId), "")
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
    hourly = "var date = new Date(x); return date.toISOString().slice(0, 16)",
    daily = "var date = new Date(x); return date.toISOString().slice(0, 10)",
    weekly = "var date = new Date(x); return date.toISOString().slice(0, 10)",
    monthly = "var date = new Date(x); return date.toISOString().slice(0, 7)",
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
