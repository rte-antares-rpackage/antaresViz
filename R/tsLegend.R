#Copyright © 2016 RTE Réseau de transport d’électricité

#' Plot an interactive legend for time series plots
#' 
#' These functions create a nice looking legend that displays values when the user
#' hovers a time series produced with plot this package. By 
#' default, the different functions already output a legend. This function
#' is mostly useful to share a unique legend between two or more time series plots.
#' 
#' @param labels vector containing the names of the times series
#' @param colors vector of colors. It must have the same length as parameter
#'   \code{labels}.
#' @param types "line" or "area" or a vector with same length as \code{labels}
#'   containing these two values.
#' @inheritParams prodStack
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
#' library(manipulateWidget)
#' 
#' combineWidgets(
#'   prodStack(mydata, areas = "fr", 
#'             main = "Production stack in France", unit = "GWh", 
#'             legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   prodStack(mydata, areas = "de", 
#'             main = "Production stack in Germany", unit = "GWh", 
#'             legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   prodStack(mydata, areas = "es", 
#'             main = "Production stack in Spain", unit = "GWh", 
#'             legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   prodStack(mydata, areas = "be", 
#'             main = "Production stack in Belgium", unit = "GWh", 
#'             legend = FALSE, legendId = 1, height = "100\%", width = "100\%"),
#'   footer = prodStackLegend(legendId = 1)
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
    style = sprintf("padding-left: 100px; height:%spx", i * 20)
  )
 
  tags$div(
    style=sprintf("position:relative;height:%spx", max(i * 20 + 20, 40)),
    tags$div(
      style = "position:absolute;top:0;bottom:0;width:100px;
",
      tags$div(
        style="text-align:center;font-weight:bold;font-size:16px",
        id = paste0("date", legendId)
      )
    ),
    tags$br(),
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

JS_updateLegend <- function(legendId, timeStep = "hourly", language = "en") {
  # Function that transform a timestamp ta a date label
  timeToLab <- switch(
    timeStep,
    hourly = paste0("var date = new Date(x); 
                var res;
                try {
                  res = date.toLocaleDateString('", language, "', { weekday: 'short', month: 'short', day: 'numeric', hour : '2-digit', minute:'2-digit', timeZone : 'UTC'  });
                } catch (e) {
                  res = date.toLocaleDateString('en', { weekday: 'short', month: 'short', day: 'numeric', hour : '2-digit', minute:'2-digit', timeZone : 'UTC'  })
                };
                // bug in Rstudio viewer / old browser
                if(res == date.toLocaleDateString()){
                  var day = date.toUTCString().slice(0, 11);
                  var h = date.toUTCString().slice(17, 22);
                  res = day + '<br/>' + h;
                }
                return res"),
    daily = paste0("var date = new Date(x); 
                var res;
                try {
                  res = date.toLocaleDateString('", language, "', { weekday: 'short', month: 'short', day: 'numeric', timeZone : 'UTC'  });
                } catch (e) {
                  res = date.toLocaleDateString('en', { weekday: 'short', month: 'short', day: 'numeric', timeZone : 'UTC'  })
                };
                // bug in Rstudio viewer / old browser
                if(res == date.toLocaleDateString()){
                     res = date.toUTCString().slice(0, 11);
                }
                return res"),
    weekly = paste0("var date = new Date(x); 
                var res;
                try {
                  res = date.toLocaleDateString('", language, "', { weekday: 'short', month: 'short', day: 'numeric', timeZone : 'UTC'  });
                } catch (e) {
                  res = date.toLocaleDateString('en', { weekday: 'short', month: 'short', day: 'numeric', timeZone : 'UTC'  })
                };
                // bug in Rstudio viewer / old browser
                if(res == date.toLocaleDateString()){
                      res = date.toUTCString().slice(0, 11);
                }
                return res"),
    monthly = paste0("var date = new Date(x);
                var res;
                try {
                  res = date.toLocaleDateString('", language, "', {month: 'long', year: 'numeric', timeZone : 'UTC' });
                } catch (e) {
                  res = date.toLocaleDateString('en', {month: 'long', year: 'numeric', timeZone : 'UTC' })
                };
                // bug in Rstudio viewer / old browser
                if(res == date.toLocaleDateString()){
                       res = date.toUTCString().slice(7, 16);
                }
                return res"),
    annual = paste0("var date = new Date(x); 
                var res;
                try {
                  res = date.toLocaleDateString('", language, "', {year: 'numeric', timeZone : 'UTC' });
                } catch (e) {
                  res = date.toLocaleDateString('en', {year: 'numeric', timeZone : 'UTC' })
                };
                // bug in Rstudio viewer / old browser
                if(res == date.toLocaleDateString()){
                      res = date.toUTCString().slice(12, 16);
                }
                return res"),
    "return x"
  )
  script <-"
function(e, timestamp, data) {
    function timeToLab(x) {%s}
    
    var tmp = document.getElementById('date%s');
    
    if(tmp){
    tmp.innerHTML = timeToLab(timestamp);
    }
    
    
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
    if (el) {
    if (Math.abs(values[k]) > 100 || values[k] === 0) {
    el.innerHTML = Math.round(values[k]);
    } else {
    el.innerHTML = values[k].toPrecision(3);
    }
    }
    }
}"
  
  JS(sprintf(script, timeToLab, legendId, legendId))
}


JS_resetLegend <- function(legendId) {
  script <- "
  function(e) {
  var tmp = document.getElementById('date%s');
  
  if(tmp){
  tmp.innerHTML = '';
  }
  
  var els = document.getElementsByClassName('legvalue');
  if(els){
  for (var i = 0; i < els.length; ++i) {els[i].innerHTML = '';}
  }
  
  }"
  
  JS(sprintf(script, legendId))
}
