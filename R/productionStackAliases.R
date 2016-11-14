# Copyright © 2016 RTE Réseau de transport d’électricité

#' show aliases for productionStack
#'
#' This function shows aliases for productionStack. These aliases can be used
#' in the \code{\link{productionStack}} function in the \code{variables} argument
#' 
#' @return
#' Print aliases. 
#' 
#' @examples 
#' productionStackAliases()
#' 
#' @export
#'
productionStackAliases <- function() {
  for (n in names(pkgEnv$prodStackAliases)) {
    alias <- pkgEnv$prodStackAliases[[n]]
    
    cat(n, "\n===========\n")
    cat(alias$description, "\n")
    
    cat("-----------\nVariables:\n")
    for (v in names(alias$variables)) {
      cat(sprintf("   %s = %s\n", 
                  v, 
                  as.character(alias$variables[[v]])))
    }
    if (length(alias$lines) > 0) {
      cat("Lines:\n")
      for (v in names(alias$lines)) {
        cat(sprintf("   %s = %s\n", 
                    v, 
                    as.character(alias$lines[[v]])))
      }
    }
    cat("\n")
  }
}
