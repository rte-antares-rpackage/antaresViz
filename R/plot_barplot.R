#' Private function that draw a barplot
#' 
#' The resulting barplot contains a bar per element which represents the average
#' value over time steps.
#' 
#' @noRd 
#'
.barplot <- function(dt, timeStep, variable, confInt = 0, maxValue,
                     colors = NULL,
                     main = NULL,
                     ylab = NULL,
                     legend = TRUE,
                     legendItemsPerRow = 5,
                     width = NULL, height = NULL, ...) {
  
  if (is.null(dt$mcYear)) {
    dt <- dt[, .(value = mean(value)), by = element] 
  } else {
    dt <- dt[, .(value = mean(value)), by = .(element, mcYear)] 
    
    if (confInt == 0) {
      dt <- dt[, .(value = mean(value)), by = .(element)]
    } else {
      uniqueElements <- sort(unique(dt$element))
      
      alpha <- (1 - confInt) / 2
      .getConfInt <- function(x) {
        q <- quantile(x, c(alpha, 1 - alpha))
        m <- mean(x)
        list(value = m, l = m - q[1], u = q[2] - m)
      }
      dt <- dt[, .getConfInt(value), by = .(element)]
    }
  }
  # print(dt)
  dt[,"element" := as.character(element)]
  variable <- paste0(variable, collapse = " ; ")
  if (is.null(ylab)) ylab <- variable
  if (is.null(main) | isTRUE(all.equal("", main))) main <- paste("Comparison of", variable)
  
  g <- plot_ly(dt,  textfont = list(color = '#000000')) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(title = main, yaxis = list(title = ylab))
  
  if (is.null(dt$l)) {
    g <- g %>% add_bars(x = ~element, y = ~value, alpha = 0.3)
  } else {
    g <- g %>% 
      add_bars(
        x = ~element, y = ~value, alpha = 0.3,
        error_y = ~list(
          type = "array",
          symmetric = FALSE,
          array = u,
          arrayminus = l
        )
      )
  }
  g <- g %>% add_text(x = ~element, y = ~value, text = ~round(value, 2))%>%
    layout(showlegend = FALSE)
  
  combineWidgets(g, width = width, height = height)
}
