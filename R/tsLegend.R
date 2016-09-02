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
    
  }
  
  tags$div(
    style = .style(width = "100%", height = h, padding = "0 4px", "font-size"= "12px"),
    content
  )
}

JS_updateLegend <- function(legendId) {
  script <-"
function(e, timestamp, data) {
  var values = {}
  
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
  
  JS(sprintf(script, legendId))
}


JS_resetLegend <- function() {
  script <- "
function(e) {
  var els = document.getElementsByClassName('legvalue');
  for (var i = 0; i < els.length; ++i) {els[i].innerHTML = '';}
}"
  
  JS(script)
}
