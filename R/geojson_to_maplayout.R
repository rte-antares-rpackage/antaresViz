library(geojsonio)
library(sf)
library(sp)
library(antaresViz)
library(data.table)


opts<-setSimulationPath("/run/user/1774400911/gvfs/smb-share:server=antrsprdsa008vm,share=per/Dossier travail création découpage zonal Europe/NT2030_zonal_no_flex_with_constraints")


zoneEuRDS <- readRDS(file ="/home/vargastat/R/maplayoutEurope_v2.rds")


#GeoJSON
geojson_file <- "/home/vargastat/R/Subzones_IoSN.geojson"
sf_object <- st_read(geojson_file)
geojson_as_sp <- as(sf_object, "Spatial")

all_coords <- data.frame(
  area = tolower(geojson_as_sp@data$name),
  x = geojson_as_sp@data$Long,
  y = geojson_as_sp@data$Lat,
  color = rep("#0080FF", times = length(geojson_as_sp@data$name)),
  geoAreaId = seq(seq_along(geojson_as_sp@data$name))
)
##geojson_as_sp object data is 123x for RDS we need zone x2
zone_geoArea_table <- data.frame(
  ZONE = all_coords$area,
  geoAreaId = all_coords$geoAreaId
)
geojson_as_sp@data<-zone_geoArea_table
setcolorder(all_coords, c("area", "x", "y", "color", "geoAreaId"))


links <- data.table(
  opts$linksDef
)

coords<-as.data.table(all_coords)
links <- links[from %in% coords$area & to %in% coords$area]

links <- merge(
  x = links,
  y = coords[, list(from = area, x0 = x, y0 = y)],
  by = "from"
)
links <- merge(
  x = links,
  y = coords[, list(to = area, x1 = x, y1 = y)],
  by = "to"
)
links
setcolorder(links, c("link", "from", "to", "x0", "y0", "x1", "y1"))


zone_layout <- list(
  coords = coords,
  links = links,
  map = geojson_as_sp,
  all_coords = all_coords
)
class(zone_layout) <- "mapLayout"
attr(zone_layout, "type") <- "areas"
#saveRDS(zone_layout, file ="/home/vargastat/R/tyndp_eu.rds")
#mapTest<-readRDS("/home/vargastat/R/tyndp_eu.rds")

mydata <- readAntares(areas =all_coords$area, links = links$link, timeStep = "daily",
                      select = "nostat", opts = opts)


#plotMap(x = mydata, mapLayout = mapTest)
plotMap(x = mydata, mapLayout = zone_layout)
