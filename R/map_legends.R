# Copyright © 2016 RTE Réseau de transport d’électricité

colorLegend <- function(title, colors, breaks) {
  options(scipen = 6)
  
  
  # Round break points in order to get unique but simple labels
  digits <- 4
  tmp <- signif(breaks, digits)
  while(any(duplicated(tmp))) {
    digits <- digits + 1
    tmp <- signif(breaks, digits)
  }
  breaks <- tmp
    
  colors <- sapply(rev(colors), function(x) {
    sprintf('<div class="legend-color" style = "background-color:%s;"></div>', x)
  })
  colors <- paste(colors, collapse = "\n")
  
  labels <- sapply(rev(breaks), function(x) {
    sprintf('<div class="legend-label">%s</div>', x)
  })
  labels <- paste(labels, collapse = "\n")
  
  sprintf(
    '
    <div class="legend-section">
      <h3>%s</h3>
      <div class="legend-colors">
        %s
      </div>
      <div class="legend-labels">
        %s
      </div>
      <div style="clear:both;"></div>
    </div>   
    ',
    title, colors, labels
  )
}

prettyValues <- function(maxValue, n = 3) {
  values <- pretty(c(0, maxValue), n = n+1)
  values <- c(values[2:(length(values) - 2)], maxValue)
  digits <- 4
  tmp <- signif(values, digits)
  while(any(duplicated(tmp))) {
    digits <- digits + 1
    tmp <- signif(values, digits)
  }
  values <- tmp
  values
}

radiusLegend <- function(title, maxRadius, maxValue) {
  options(scipen = 6)
  
  values <- prettyValues(maxValue)
  radius <- sqrt(values) / sqrt(maxValue) * maxRadius
  
  circles <- sprintf(
    '<svg width="%s" height="%s">
      <circle cx="%s" cy="%s" r="%s" stroke="#ccc" fill="none"></circle>
    </svg>',
    maxRadius * 2, radius * 2, maxRadius, radius, radius 
  )
  circles <- paste(rev(circles), collapse = "\n")
  
  labels <- sprintf(
    '<div class="legend-label" style="line-height:%spx">%s</div>',
    radius * 2 + 4, values
  )
  labels <- paste(rev(labels), collapse = "")
  
  sprintf(
    '
    <div class="legend-section">
      <h3>%s</h3>
      <div class="legend-symbols">
        %s
      </div>
      <div class="legend-labels">
        %s
      </div>
    </div>
    ',
    title, circles, labels
  )
}

lineWidthLegend <- function(title, maxWidth, maxValue) {
  options(scipen = 6)
  
  values <- prettyValues(maxValue)
  widths <- values / maxValue * maxWidth;
  
  lines <- sprintf(
    '<svg width="%s" height="%s">
      <line x1="%s" y1="%s" x2="%s" y2="%s" stroke-width="%s"></line>
    </svg>',
    30, maxWidth * 2, 0, maxWidth, 30, maxWidth, widths
  )
  lines <- paste(rev(lines), collapse = "\n")
  
  labels <- sprintf(
    '<div class="legend-label" style="line-height:%spx">%s</div>',
    maxWidth * 2 + 2, values
  )
  labels <- paste(rev(labels), collapse = "")
  
  sprintf(
    '
    <div class="legend-section">
      <h3>%s</h3>
      <div class="legend-symbols">
        %s
      </div>
      <div class="legend-labels">
        %s
      </div>
    </div>
    ',
    title, lines, labels
  )
}

barChartLegend <- function(labels, title = "", colors = NULL) {
  if (is.null(colors)) {
    colors <- DEFAULT_CAT_COLORS
  }
  
  colors <- colors[1:length(labels)]
  
  colors <- sapply(colors, function(x) {
    sprintf('<div class="legend-color-cat" style = "background-color:%s;"></div>', x)
  })
  colors <- paste(colors, collapse = "\n")
  
  labels <- sapply(labels, function(x) {
    sprintf('<div class="legend-label">%s</div>', x)
  })
  labels <- paste(labels, collapse = "\n")
  
  sprintf(
    '
    <div class="legend-section">
      <h3>%s</h3>
      <div class="legend-colors-cat">
        %s
      </div>
      <div class="legend-labels">
        %s
      </div>
      <div style="clear:both;"></div>
    </div>   
    ',
    title, colors, labels
  )
  
}
