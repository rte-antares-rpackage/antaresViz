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
      cat("Curves:\n")
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

.productionStackAliases <- list(
  eco2mix = list(
    description = "Production stack used on Eco2mix website: 
http://www.rte-france.com/fr/eco2mix/eco2mix-mix-energetique",
    variables = alist(
      pumpedStorage  = PSP,
      exports = - (BALANCE + `ROW BAL.`),
      wind = WIND,
      solar = SOLAR,
      nuclear = NUCLEAR,
      hydraulic = `H. ROR` + `H. STOR`,
      gas = GAS,
      coal = COAL + LIGNITE,
      fuel = `MIX. FUEL` + OIL + `MISC. DTG` + `MISC. NDG`
    ),
    colors = rgb(
      red =   c( 17, 150, 116, 242, 245,  39, 243, 172, 131),
      green = c( 71, 150, 205, 116, 179, 114,  10, 140,  86),
      blue =  c(185, 150, 185,   6,   0, 178,  10,  53, 162),
      maxColorValue = 255
    ),
    lines = alist(
      load = LOAD
    ),
    lineColors = c("#000000")
  ),
  
  test = list(
    description = "Test alias",
    variables = alist(
      renewable = WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG`,
      thermal = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`
    ),
    colors = c("green", gray(0.3)),
    lines = alist(
      goalRenewable = LOAD * 0.23
    ),
    lineColors = "#42EB09"
  )
  
)