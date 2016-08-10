#' @import data.table
#' @import plyr

pkgEnv <- antaresRead:::pkgEnv
# Private variables accessible only by functions from the package


#Path file for the GraphicalCharter 
#Sep = TAB
#the use of system.file create a bug... -> don't use it
pkgEnv$colorsVariablesTable<-fread(input="inst/GraphicalCharter.csv")

pkgEnv$eco2mixVaribales<-alist(
  pumpedStorage  = PSP,
  minusBalance = -(BALANCE + `ROW BAL.`),
  bioenergie = `MISC. NDG`, 
  wind = WIND,
  solar = SOLAR,
  nuclear = NUCLEAR,
  hydraulic = `H. ROR` + `H. STOR`,
  gas = GAS,
  coal = COAL + LIGNITE,
  fuel = `MIX. FUEL` + OIL ,
  other = `MISC. DTG`
)

pkgEnv$eco2mixLines<-alist(
  load = LOAD,
  totalProduction= NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`+WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG`+PSP
)


pkgEnv$thermalFirstVaribales<-alist(
  pumpedStorage  = PSP,
  minusBalance = -(BALANCE + `ROW BAL.`),
  nuclear = NUCLEAR,
  lignite=LIGNITE,
  coal = COAL,
  gas = GAS,
  oil = OIL,
  mixFuel=`MIX. FUEL`,
  other = `MISC. DTG`,
  bioenergie = `MISC. NDG`,
  wind = WIND,
  solar = SOLAR,
  hydraulicRor=`H. ROR`,
  hydraulicStro=`H. STOR`
)


pkgEnv$testVaribales<-alist(
  renewable = WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG`,
  thermal = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`
)

pkgEnv$netLoadVaribales<-alist(
  pumpedStorage  = PSP,
  minusBalance = -(BALANCE + `ROW BAL.`),
  nuclear = NUCLEAR,
  lignite=LIGNITE,
  coal = COAL,
  gas = GAS,
  oil = OIL,
  mixFuel=`MIX. FUEL`,
  other = `MISC. DTG`,
  hydraulicStro=`H. STOR`
)

pkgEnv$netLoadLines<-alist(
  netLoad = LOAD - `MISC. NDG`- WIND - SOLAR - `H. ROR`
)


pkgEnv$mustRunVaribales<-alist(
  pumpedStorage  = PSP,
  minusBalance = -(BALANCE + `ROW BAL.`),
  other = `MISC. DTG`,
  nuclear = NUCLEAR,
  lignite=LIGNITE,
  coal = COAL,
  gas = GAS,
  oil = OIL,
  mixFuel=`MIX. FUEL`,
  hydraulicStro=`H. STOR`,
  bioenergie = `MISC. NDG`, 
  wind = WIND,
  solar = SOLAR,
  hydraulicRor=`H. ROR`
)

pkgEnv$mustRunLines<-alist(
  thermalAvailability = `AVL DTG`
)