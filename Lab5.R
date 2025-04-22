library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(tmap)
library(classInt)
library(RColorBrewer)
library(spdep)
library(ggplot2)
library(dplyr)
library(leafem)
library(terra)

#Task 1
counties <- sf::read_sf("../Data/County_Boundaries.shp")
counties %>% sf::st_is_valid()
counties <- counties %>% sf::st_make_valid()

bmps <- read_csv("../Data/BMPreport2016_landbmps.csv")
bmps <- bmps %>%
  mutate(GEOID10 = substr(GeographyName, 1, 5)) %>%
  group_by(GEOID10) %>%
  summarize(Total_Cost = sum(Cost, na.rm = T))

counties_bmps <- left_join(counties, bmps, by = "GEOID10")
# tm_shape(counties_bmps)+
  # tm_polygons(fill = "Cost")

breaks <- classIntervals(counties_bmps$Total_Cost, n = 5, style = "equal")$brks


slaycolors <- brewer.pal(5, "BuPu") 
pal <- colorBin(palette = slaycolors, bins = breaks, domain = counties_bmps$Total_Cost)

leaflet(data=counties_bmps) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = counties_bmps,
              fillColor = ~pal(Total_Cost),
              stroke = TRUE,
              fillOpacity = 0.9,
              color = "black",
              weight = 0.3,
              popup = ~paste0("Total Cost: $", formatC(Total_Cost, format = "f", big.mark = ",", digits = 0)),
              label = ~paste0("Total BMP Cost: $", formatC(Total_Cost, format = "f", big.mark = ",", digits = 0)),) %>%
  addLegend(pal = pal,
            values = ~Total_Cost,
            title = "Cost of BMPs",
            position = "bottomright",
            labFormat = labelFormat(prefix = "$", big.mark = ",", digits = 0))
  
#Task 2
nypanjmades <- sf::read_sf("../data/nypanjmade.shp")
#make neighbors and spatial weights
nb <- poly2nb(nypanjmades)
lw <- nb2listw(nb, style = "W")
#do the morans
local_moran <- localmoran(nypanjmades$B01001Ie1, lw)

# standardize values
z <- scale(nypanjmades$B01001Ie1)[,1]
lag_z <- lag.listw(lw, z)

#making quadrants
p_values <- local_moran[, "Pr(z != E(Ii))"]
lisa_quadrants <- case_when(
  z > 0 & lag_z > 0 & p_values < 0.05 ~ "HH",
  z > 0 & lag_z < 0 & p_values < 0.05 ~ "HL",
  z < 0 & lag_z < 0 & p_values < 0.05 ~ "LL",
  z < 0 & lag_z > 0 & p_values < 0.05 ~ "LH",
  TRUE ~ "Not significant"
)
nypanjmades$lisa_quad <- factor(
  lisa_quadrants,
  levels = c("HH", "HL", "LL", "LH", "Not significant")
)

quad_palette <- colorFactor(
  palette = c("red", "orange", "blue", "lightblue", "white"),
  domain = nypanjmades$lisa_quad
)

leaflet(nypanjmades) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Streets") %>%
  addPolygons(
    fillColor = ~quad_palette(lisa_quad),
    color = "black",
    weight = 1,
    popup = ~paste0(
      "<strong>County:</strong> ", NAME, "<br>",
      "<strong>LISA Quadrant:</strong> ", lisa_quad, "<br>",
      "<strong>p-value:</strong> ", p_values
    ),
    label = ~paste0(NAME, ": ", lisa_quad),
    group = "LISA Clusters"
  ) %>%
  addLegend(
    pal = quad_palette,
    values = ~lisa_quad,
    title = "LISA Cluster Type",
    position = "bottomright"
  ) %>%
 addLayersControl(
    baseGroups = c("Light", "Satellite", "Streets"),
    overlayGroups = c("LISA Clusters"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Task 3
nz <- st_as_sf(nz) 
nz <- st_transform(nz, crs = 4326)
nz_elev <- rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))
nz_elev_wgs84 <- project(nz_elev, "EPSG:4326")

pal <- colorNumeric(
  palette = terrain.colors(25),
  domain = values(nz_elev_wgs84),
  na.color = "transparent"
)



leaflet() %>%
  addTiles() %>%
  addGraticule(interval = 5, style = list(color = "#999", weight = 0.5)) %>%
  setView(lng = 172.5, lat = -41, zoom = 5) %>%
  addRasterImage(nz_elev_wgs84, colors = pal, opacity = 0.7, project = TRUE, group = "Elevation (m)") %>%
  addLegend(pal = pal, values = values(nz_elev_wgs84),
            title = "Elevation (m)", position = "bottomright") %>%
  addPolygons(data = nz, color = "black", weight = 2, fill = FALSE, group = "nz") %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE)) %>%
  addMouseCoordinates() %>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Center Map",
                           onClick=JS("function(btn, map){ map.setView([ -41, 172.5 ], 5); }"))) %>%
  addControl("<b>Elevation Map of New Zealand</b>", position = "topright") %>%
  addLayersControl(
    overlayGroups = c("Elevation (m)", "nz"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(toggleDisplay = TRUE)






