#' @import data.table
#' @import plyr

pkgEnv <- antaresRead:::pkgEnv
# Private variables accessible only by functions from the package


#Path file for the GraphicalCharter 
#Sep = TAB
#the use of system.file create a bug... -> don't use it
pkgEnv$colorsVariablesTable<-fread(input="inst/GraphicalCharter.csv")
#cols <- 'Formula'
#pkgEnv$colorsVariablesTable[,(cols):=lapply(.SD, as.factor),.SDcols=cols]

.getFormula<-function(NameVariable){
  parse(text = pkgEnv$colorsVariablesTable[namesVariables==NameVariable, .(Formula)]$Formula)
}

.getCharacterFormula<-function(NameVariable){
  pkgEnv$colorsVariablesTable[namesVariables==NameVariable, .(Formula)]$Formula
}

####################### begin Eco2Mix Alias #####################################
pkgEnv$eco2mixVaribales<-alist(
  pumpedStorage  = eval(.getFormula("pumpedStorage")),
  minusBalance = eval(.getFormula("minusBalance")),
  bioenergie = eval(.getFormula("bioenergie")), 
  wind = eval(.getFormula("wind")),
  solar = eval(.getFormula("solar")),
  nuclear = eval(.getFormula("nuclear")),
  hydraulic = eval(.getFormula("hydraulic")),
  gas = eval(.getFormula("gas")),
  coal = eval(.getFormula("coal")),
  fuel = eval(.getFormula("fuel")),
  other = eval(.getFormula("other"))
)

pkgEnv$eco2mixVaribalesCharacter<-alist(
  pumpedStorage  = .getCharacterFormula("pumpedStorage"),
  minusBalance = .getCharacterFormula("minusBalance"),
  bioenergie = .getCharacterFormula("bioenergie"), 
  wind = .getCharacterFormula("wind"),
  solar = .getCharacterFormula("solar"),
  nuclear = .getCharacterFormula("nuclear"),
  hydraulic = .getCharacterFormula("hydraulic"),
  gas = .getCharacterFormula("gas"),
  coal = .getCharacterFormula("coal"),
  fuel = .getCharacterFormula("fuel"),
  other = .getCharacterFormula("other")
)

pkgEnv$eco2mixLines<-alist(
  load = eval(.getFormula("load")),
  totalProduction= eval(.getFormula("totalProduction"))
)

pkgEnv$eco2mixLinesCharacter<-alist(
  load = .getCharacterFormula("load"),
  totalProduction= .getCharacterFormula("totalProduction")
)


####################### end Eco2Mix Alias #####################################

####################### begin ThermalFirst Alias #####################################

pkgEnv$thermalFirstVaribales<-alist(
  pumpedStorage  = eval(.getFormula("pumpedStorage")),
  minusBalance = eval(.getFormula("minusBalance")),
  nuclear = eval(.getFormula("nuclear")),
  lignite= eval(.getFormula("lignite")),
  coal = eval(.getFormula("coal")),
  gas = eval(.getFormula("gas")),
  oil = eval(.getFormula("oil")),
  mixFuel= eval(.getFormula("mixFuel")),
  other = eval(.getFormula("other")),
  bioenergie = eval(.getFormula("bioenergie")),
  wind = eval(.getFormula("wind")),
  solar = eval(.getFormula("solar")),
  hydraulicRor= eval(.getFormula("hydraulicRor")),
  hydraulicStro= eval(.getFormula("hydraulicStro"))
)

pkgEnv$thermalFirstVaribalesCharacter<-alist(
  pumpedStorage  = .getCharacterFormula("pumpedStorage"),
  minusBalance = .getCharacterFormula("minusBalance"),
  nuclear = .getCharacterFormula("nuclear"),
  lignite= .getCharacterFormula("lignite"),
  coal = .getCharacterFormula("coal"),
  gas = .getCharacterFormula("gas"),
  oil =.getCharacterFormula("oil"),
  mixFuel= .getCharacterFormula("mixFuel"),
  other =.getCharacterFormula("other"),
  bioenergie = .getCharacterFormula("bioenergie"),
  wind = .getCharacterFormula("wind"),
  solar = .getCharacterFormula("solar"),
  hydraulicRor= .getCharacterFormula("hydraulicRor"),
  hydraulicStro= .getCharacterFormula("hydraulicStro")
)

####################### end ThermalFirst Alias #####################################

####################### begin test Alias #####################################

pkgEnv$testVaribales<-alist(
  renewable = eval(.getFormula("renewable")),
  thermal = eval(.getFormula("thermal"))
)

pkgEnv$testVaribalesCharacter<-alist(
  renewable = .getCharacterFormula("renewable"),
  thermal = .getCharacterFormula("thermal")
)

####################### end  test Alias #####################################

####################### begin netLoad Alias #####################################

pkgEnv$netLoadVaribales<-alist(
  pumpedStorage  = eval(.getFormula("pumpedStorage")),
  minusBalance = eval(.getFormula("minusBalance")),
  nuclear = eval(.getFormula("nuclear")),
  lignite= eval(.getFormula("lignite")),
  coal = eval(.getFormula("coal")),
  gas = eval(.getFormula("gas")),
  oil = eval(.getFormula("oil")),
  mixFuel= eval(.getFormula("mixFuel")),
  other = eval(.getFormula("other")),
  hydraulicStro= eval(.getFormula("hydraulicStro"))
)

pkgEnv$netLoadVaribalesCharacter<-alist(
  pumpedStorage  = .getCharacterFormula("pumpedStorage"),
  minusBalance = .getCharacterFormula("minusBalance"),
  nuclear = .getCharacterFormula("nuclear"),
  lignite= .getCharacterFormula("lignite"),
  coal = .getCharacterFormula("coal"),
  gas = .getCharacterFormula("gas"),
  oil = .getCharacterFormula("oil"),
  mixFuel= .getCharacterFormula("mixFuel"),
  other = .getCharacterFormula("other"),
  hydraulicStro= .getCharacterFormula("hydraulicStro")
)


pkgEnv$netLoadLines<-alist(
  netLoad = eval(.getFormula("netLoad"))
)

pkgEnv$netLoadLinesCharacter<-alist(
  netLoad = .getCharacterFormula("netLoad")
)

####################### end netLoad Alias #####################################


####################### begin mustRun Alias #####################################

pkgEnv$mustRunVaribales<-alist(
  pumpedStorage  = eval(.getFormula("pumpedStorage")),
  minusBalance = eval(.getFormula("minusBalance")),
  mustRunTotal = eval(.getFormula("mustRunTotal")),
  thermalDispatchable = eval(.getFormula("thermalDispatchable")),
  hydraulicDispatchable=  eval(.getFormula("hydraulicDispatchable")),
  renewableNoDispatchable = eval(.getFormula("renewableNoDispatchable"))
)

pkgEnv$mustRunVaribalesCharacter<-alist(
  pumpedStorage  = .getCharacterFormula("pumpedStorage"),
  minusBalance = .getCharacterFormula("minusBalance"),
  mustRunTotal = .getCharacterFormula("mustRunTotal"),
  thermalDispatchable = .getCharacterFormula("thermalDispatchable"),
  hydraulicDispatchable=  .getCharacterFormula("hydraulicDispatchable"),
  renewableNoDispatchable = .getCharacterFormula("renewableNoDispatchable")
)

pkgEnv$mustRunLines<-alist(
  thermalAvailability = eval(.getFormula("thermalAvailability"))
)

pkgEnv$mustRunLinesCharacter<-alist(
  thermalAvailability = .getCharacterFormula("thermalAvailability")
)
####################### end mustRun Alias #####################################