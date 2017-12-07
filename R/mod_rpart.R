# Copyright © 2016 RTE Réseau de transport d’électricité

#' Make rpart from antares data
#' 
#' @param data an antaresData after use of \code{\link[antaresProcessing]{mergeAllAntaresData}}
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath("Mystud", 1)
#' mydata <- readAntares(areas = "all", select = "OIL")
#' mydata <- mergeAllAntaresData(mydata)
#' modRpart(mydata)
#' }
#' 
#' @export
modRpart <- function(data){
  visNetwork::visTreeEditor(data.frame(data))
}