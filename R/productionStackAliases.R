productionStackAliases <- function() {
  for (n in names(.productionStackAliases)) {
    alias <- .productionStackAliases[[n]]
    
    cat(n, "\n===========\n")
    cat(alias$description, "\n")
    
    cat("-----------\nVariables:\n")
    for (v in names(alias$variables)) {
      cat(sprintf("   %s = %s\n", 
                  v, 
                  deparse(alias$variables[[v]])))
    }
    if (!is.null(alias$lines)) {
      cat("Lines:\n")
      for (v in names(alias$lines)) {
        cat(sprintf("   %s = %s\n", 
                    v, 
                    deparse(alias$lines[[v]])))
      }
    }
    cat("\n")
  }
}


# List of aliases for parameter "variables" in function productionStack()
#
# Each element has five elements:
# - description: A concise description of the production stack.
# - variables:   Definition of the variables to draw
# - colors:      Vector of colors with same length as "variables"
# - lines:       (optional) Definition of curves to draw on top of the stack
# - lineColors:  Vector of colors with same length as lines. Mandatory only if
#                "lines" is set

.productionStackAliases <- alist(
  eco2mix = list(
    description = "Production stack used on Eco2mix website: 
http://www.rte-france.com/fr/eco2mix/eco2mix-mix-energetique",
    variables = pkgEnv$eco2mixVaribales,
    colors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$eco2mixVaribales), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
     ),
     lines = pkgEnv$eco2mixLines,
     lineColors = rgb(
       pkgEnv$colorsVariablesTable[ match(names(pkgEnv$eco2mixLines), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
       maxColorValue = 255
    )
  ),
  
  test = list(
    description = "Test alias",
    variables = pkgEnv$testVaribales,
    colors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$testVaribales), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    ),
    lines <- alist(
      goalRenewable = LOAD*0.23
    ),
    lineColors = "#42EB09"
  ),

  thermalFirst = list(
    description = "thermal first",
    variables = pkgEnv$thermalFirst,
    colors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$thermalFirst), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    )
    ), 
  
  netLoad = list(
    description = "netLoad",
    variables = pkgEnv$netLoadVaribales,
    colors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$netLoadVaribales), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    ),
    lines = pkgEnv$netLoadLines,
    lineColors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$netLoadLines), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    )
  ), 
  mustRun = list(
    description = "must-run",
    variables = pkgEnv$mustRunVaribales,
    colors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$mustRunVaribales), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    ),
    lines = pkgEnv$mustRunLines,
    lineColors = rgb(
      pkgEnv$colorsVariablesTable[ match(names(pkgEnv$mustRunLines), pkgEnv$colorsVariablesTable$namesVariables),.(colorsRed, colorsGreen, colorsBlue) ],
      maxColorValue = 255
    )
  )
)