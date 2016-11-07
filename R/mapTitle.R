addTitle <- function(map, main = "") {
  if (is.null(main) || is.na(main) || main == "") return(map)
  
  newLines <- gregexpr("\n", main)[[1]]
  if (newLines != -1) {
    nLines <- 1 + length(newLines)
    main <- gsub("\n", "<br/>", main)
  } else {
    nLines <- 1
  }
  
  map %>% htmlwidgets::onRender(JS_addTitle, list(size = nLines * 28, title = main))
}

JS_addTitle <- JS('
function(el, x, data) {
  var htmlwidgetContainer = el.parentElement; 
  
  var container = document.createElement("div");
  container.id = "container";
  container.style.height = el.style.height;
  container.style.width = el.style.width;
  container.style.position = "relative";
  
  el.style.position = "absolute";
  el.style.top = data.size + "px";
  el.style.bottom = "0";
  el.style.height = "auto";
  container.appendChild(el);
  
  var title = document.createElement("div");
  title.style.height = data.size + "px";
  title.style.position = "absolute";
  title.style.top = "0";
  title.style.left = "0";
  title.style.right = "0";
  title.style.fontSize = "20px";
  title.style.textAlign = "center";
  title.style.fontWeight = "bold";
  title.style.lineHeight = "28px";
  title.style.fontFamily = "sans-serif";
  title.innerHTML = data.title;
  container.appendChild(title);
  
  htmlwidgetContainer.appendChild(container);
  
  this.resize();
}
')