## ----loadPackage, message = FALSE, warning=FALSE-------------------------
library(antaresViz)

## ----loadData, message = FALSE-------------------------------------------
load("data_for_antaresViz_vignette_extralight.Rdata")

## ----barplot-------------------------------------------------------------
plot(data_annual_synthesis, table = "districts" ,variable = "LOLD", type = "barplot", elements = c("00_a", "00_b", "00_c", "00_d", "00_e", "00_f", "00_g", "00_h", "00_i"), interactive = FALSE, width = "100%", height = 400)

## ----ts------------------------------------------------------------------
plot(data_hourly_allmc, table = "areas" ,variable = "LOAD", type = "ts", elements = "23_b", confInt = 0.95, dateRange = c("2018-01-08", "2018-01-14"), width = "100%", height = 400, interactive = FALSE)

## ----density-------------------------------------------------------------
plot(data_hourly_synthesis, table = "areas" ,variable = "WIND", type = "density", elements = c("01_a", "02_a"), interactive = FALSE, width = "100%", height = 400)

## ----heatmap-------------------------------------------------------------
plot(data_hourly_synthesis_1year, table = "links", variable = "CONG. PROB +", type = "heatmap", elements = "25_c - 26_d", interactive = FALSE, width = "100%", height = 400, main = "Congestion probability")

## ----prodStack-----------------------------------------------------------
prodStack(data_hourly_synthesis, stack = "eco2mix", areas = "37_h", dateRange = c("2018-01-08", "2018-01-21"), main = "Production stack", unit = "GWh",  interactive = FALSE, width = "100%", height = 500)

## ----exchangeStack-------------------------------------------------------
exchangesStack(data_hourly_synthesis, area = "37_h", dateRange = c("2018-01-08", "2018-01-21"), main = "Import/Export of area 37_h", unit = "GWh",  interactive = FALSE, width = "100%", height = 500)

