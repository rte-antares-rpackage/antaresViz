# Copyright © 2016 RTE Réseau de transport d’électricité

#' Aliases of production stacks
#'
#' This function shows aliases for \code{\link{prodStack}}. These aliases can be used
#' in the \code{\link{prodStack}} function in the \code{variables} argument
#' 
#' @return
#' Print aliases. 
#' 
#' @examples 
#' prodStackAliases()
#' 
#' @export
#' 
prodStackAliases <- function() {
  for (n in names(pkgEnv$prodStackAliases)) {
    alias <- pkgEnv$prodStackAliases[[n]]
    
    cat("\n", n, "\n===========\n")
    cat(alias$description, "\n")
    
    # Stacks definition
    names <- names(alias$variables)
    formulas <- vapply(alias$variables, FUN.VALUE = character(1), function(x) {
      as.character(as.expression(x))
    })
    formulas <- sprintf('"%s" = %s', names, formulas)
    
    
    cat("-----------\n  variables = alist(\n    ")
    cat(paste(formulas, collapse = ",\n    "))
    cat("\n  )")
    
    # Stack colors
    colors <- sprintf('"%s"', alias$colors)
    cat(sprintf(",\n\n  colors = c(%s)", paste(colors, collapse = ", ")))
    
    if (length(alias$lines) > 0) {
      # Lines definition
      names <- names(alias$lines)
      formulas <- vapply(alias$lines, FUN.VALUE = character(1), function(x) {
        as.character(as.expression(x))
      })
      formulas <- sprintf('"%s" = %s', names, formulas)

      cat(",\n\n  lines = alist(\n    ")
      cat(paste(formulas, collapse = ",\n    "))
      cat("\n  )")
      
      # Line colors
      colors <- sprintf('"%s"', alias$lineColors)
      cat(sprintf(",\n\n  lineColors = c(%s)", paste(colors, collapse = ", ")))
    }
    cat("\n")
  }
}
