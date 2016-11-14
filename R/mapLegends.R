# Copyright © 2016 RTE Réseau de transport d’électricité

colorLegend <- function(title, colors, breaks) {
  options(scipen = 6)
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
  c(values[2:(length(values) - 2)], maxValue)
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
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
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

polarChartLegend <- function(title = "") {
  sprintf(
    '
    <div class="legend-section">
      <h3>%s</h3>
      <svg id="polar-chart-legend" width="200" height="60"></svg>
    </div>
    ',
    title
  )
}

polarChartLegendJS <- function(labels, colors = NULL) {
  if (is.null(colors)) {
    colors <- '["#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf"]'
  } else {
    colors <- jsonlite::toJSON(colors)
  }
  
  labels <- jsonlite::toJSON(labels)
  
  sprintf('
  var labels = %s;
  var colors = %s;
  var radius = 15;
  
  var svg = d3.select("#polar-chart-legend")
  var g = svg.append("g")
    .attr("transform", "translate(" + 100 + "," + 30 + ")");
  
  // Draw the polar chart
  var pie = d3.pie().value(function(d) {return 1;});
  var arc = d3.arc().innerRadius(0).outerRadius(radius);
  var innerArc = d3.arc().innerRadius(radius).outerRadius(radius);
  var outerArc = d3.arc().innerRadius(radius * 1.5).outerRadius(radius * 1.5);
  var color = d3.scaleOrdinal(colors);
  
  g.selectAll("path")
    .data(pie(labels))
    .enter()
    .append("path")
    .attr("d", arc)
    .attr("fill", function(d, i) {return color(i)});
  
  // Add labels
  var txt = svg.append("g")
    .attr("transform", "translate(" + 100 + "," + 30 + ")");
  
  function midAngle(d){
    return d.startAngle + (d.endAngle - d.startAngle)/2;
  }
  
  txt.selectAll("text")
    .data(pie(labels))
    .enter()
    .append("text")
    .text(function(d){return d.data;})
    .attr("dy", "2.5px")
    .attr("transform", function(d) {
      var pos = outerArc.centroid(d);
      pos[0] = radius * 2 * (midAngle(d) < Math.PI ? 1 : -1);
      return "translate("+ pos +")";
    })
    .attr("text-anchor", function(d) {
      return midAngle(d) < Math.PI ? "start" : "end";
    })
    .attr("class", "legend-label");
  
  // add lines
  var lines = svg.append("g")
    .attr("transform", "translate(" + 100 + "," + 30 + ")");
  
  lines.selectAll("polyline")
    .data(pie(labels))
    .enter()
    .append("polyline")
    .attr("fill", "none")
    .attr("stroke", "#ccc")
    .attr("points", function(d) {
      var pos = outerArc.centroid(d);
      pos[0] = radius * 1.5 * (midAngle(d) < Math.PI ? 1 : -1);
      return [innerArc.centroid(d), outerArc.centroid(d), pos];
    });
  
  ', labels, colors)
}
