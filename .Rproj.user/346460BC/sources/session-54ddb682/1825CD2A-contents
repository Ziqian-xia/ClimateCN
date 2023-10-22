# Load necessary libraries
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(rmapshaper)

# Prepare datasets for 2010 and 2023
famiall23 %>% filter(ty == 2010) -> famiall2010
famiall23 %>% filter(ty == 2023) -> famiall2023

# Transform to long-lat CRS
famiall2010 <- st_transform(famiall2010, 4326)
famiall2023 <- st_transform(famiall2023, 4326)

# Simplify geometry
famiall2010_simplified <- ms_simplify(famiall2010, keep = 0.05)
famiall2023_simplified <- ms_simplify(famiall2023, keep = 0.05)



# Define label creation function
create_label <- function(data) {
  label <- paste0("<strong>Province: </strong>", data$state,
                  "<br><strong>Percentage: </strong>", round(data$ebma * 100, 2), "%")
  return(label)
}

# ... rest of your code above

# Revised palette to cover the range of ebma values for both years
palette2010 <- colorBin(palette = colorRampPalette(c("#e0f3f8", "#fee090", "#d73027"))(12),
                        bins = round(seq((min(famiall2010$ebma, na.rm = TRUE)-0.01) * 100,
                                   (max(famiall2023$ebma, na.rm = TRUE)+0.01) * 100,
                                   length.out = 13),2),
                        domain = famiall2010$ebma * 100)

# Create Leaflet map
pifull<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add 2010 polygons
  addPolygons(data = famiall2010_simplified,
              fillColor = ~palette2010(ebma * 100),
              fillOpacity = 0.5,  # Adjust opacity to see overlapping areas
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(ebma * 100, 2), "%"),
              popup = ~create_label(famiall2010),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              group = "Year 2010") %>%
  # Add 2023 polygons
  addPolygons(data = famiall2023_simplified,
              fillColor = ~palette2010(ebma * 100),  # Use the same palette for 2023
              fillOpacity = 0.5,  # Adjust opacity to see overlapping areas
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(ebma * 100, 2), "%"),
              popup = ~create_label(famiall2023),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              group = "Year 2023") %>%
  # Add layers control
  addLayersControl(
    overlayGroups = c("Year 2010", "Year 2023"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add shared legend
  addLegend(
    pal = palette2010,
    values = c(famiall2010$ebma, famiall2023$ebma) * 100,  # Directly specify values
    title = "Perceived Impact (%)",
    position = "bottomright",
    labFormat = labelFormat(suffix = "%")
  )%>%
  addControl(
    html = "<div style='font-size:18px; font-weight:bold;'>Perceived Impact of Climate Change (2010 & 2023)</div>",
    position = "bottomleft"
  )




#######################
# Revised palette to cover the range of ebma values for both years
palette2010 <- colorBin(palette = colorRampPalette(c("#e0f3f8", "#fee090", "#d73027"))(12),
                        bins = round(seq((min(mostall2010$ebma, na.rm = TRUE)-0.01) * 100,
                                   (max(mostall2023$ebma, na.rm = TRUE)+0.01) * 100,
                                   length.out = 13),2),
                        domain = mostall2010$ebma * 100)

create_label <- function(data) {
  label <- paste0("<strong>Province: </strong>", data$state,
                  "<br><strong>Percentage: </strong>", round(data$ebma * 100, 2), "%",
                  "<br><strong>Region: </strong>", data$region)
  return(label)
}


# Create Leaflet map
ppfull<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add 2010 polygons
  addPolygons(data = mostall2010_simplified,
              fillColor = ~palette2010(ebma * 100),
              fillOpacity = 0.5,  # Adjust opacity to see overlapping areas
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(ebma * 100, 1), "%"),
              popup = ~create_label(mostall2010),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              group = "Year 2010") %>%
  # Add 2023 polygons
  addPolygons(data = mostall2023_simplified,
              fillColor = ~palette2010(ebma * 100),  # Use the same palette for 2023
              fillOpacity = 0.5,  # Adjust opacity to see overlapping areas
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(ebma * 100, 2), "%"),
              popup = ~create_label(mostall2023),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              group = "Year 2023") %>%
  # Add layers control
  addLayersControl(
    overlayGroups = c("Year 2010", "Year 2023"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add shared legend
  addLegend(
    pal = palette2010,
    values = c(famiall2010$ebma, famiall2023$ebma) * 100,  # Directly specify values
    title = "Perceived Priority (%)",
    position = "bottomright",
    labFormat = labelFormat(suffix = "%")
  )%>%
  addControl(
    html = "<div style='font-size:18px; font-weight:bold;'>Perceived Priority of Climate Change (2010 & 2023)</div>",
    position = "bottomleft"
  )

save(pifull, file = "pifull.RData")
save(ppfull, file = "ppfull.RData")


######################################################################
# Follow the example before, but now for city level map

mapdata2 <- st_transform(mapdata2, crs = 4326)
mapdata2_simplified <- ms_simplify(mapdata2, keep = 0.05)
# Convert mrp to a factor variable with 10 levels
mapdata2$mrp_bin <- cut(mapdata2$mrp, breaks = seq(min(mapdata2$mrp, na.rm = TRUE), max(mapdata2$mrp, na.rm = TRUE), length.out = 11), include.lowest = TRUE, labels = FALSE)

# Generate custom labels
label_list <- round(seq(min(mapdata2$mrp, na.rm = TRUE), max(mapdata2$mrp, na.rm = TRUE), length.out = 10), 2)
label_list <- paste0(label_list*100, "%")




# Color palette
num_colors <- 12
palette <- colorBin(palette = colorRampPalette(c("#ffffff","#de2d26"))(num_colors),
                    bins = round(seq((min(mapdata2$mrp, na.rm = TRUE)-0.01) * 100,
                                     (max(mapdata2$mrp, na.rm = TRUE)+0.01) * 100,
                                     length.out = num_colors + 1),2),
                    domain = mapdata2$mrp * 100)


create_label0 <- function(data) {
  label <- paste0("<strong>Province: </strong>", data$name,
                  "<br><strong>Percentage: </strong>", round(data$mrp * 100, 2), "%")
  return(label)
}


cpi2023<-leaflet(mapdata2_simplified) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~palette(mrp * 100),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(mrp * 100, 2), "%"),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~create_label0(mapdata2)) %>%
  addLegend(pal = palette,
            values = ~mrp * 100,
            title = "Perceived Impact (%)",
            position = "bottomright",
            labFormat = labelFormat(suffix = "%"))%>%
  addControl(
    html = "<div style='font-size:18px; font-weight:bold;'>Perceived Impact of Climate Change (city-level)</div>",
    position = "bottomleft"
  )

leaflet_map


######## Perceived Priority of Climate Change City Level

# Follow the example before, but now for city level map

mapdata1 <- st_transform(mapdata1, crs = 4326)
mapdata1_simplified <- ms_simplify(mapdata1, keep = 0.05)
# Convert mrp to a factor variable with 10 levels
mapdata1$mrp_bin <- cut(mapdata1$mrp, breaks = seq(min(mapdata1$mrp, na.rm = TRUE), max(mapdata1$mrp, na.rm = TRUE), length.out = 11), include.lowest = TRUE, labels = FALSE)

# Generate custom labels
label_list <- round(seq(min(mapdata1$mrp, na.rm = TRUE), max(mapdata1$mrp, na.rm = TRUE), length.out = 10), 2)
label_list <- paste0(label_list*100, "%")




# Color palette
num_colors <- 12
palette <- colorBin(palette = colorRampPalette(c("#ffffff","#076585"))(num_colors),
                    bins = round(seq((min(mapdata1$mrp, na.rm = TRUE)-0.01) * 100,
                                     (max(mapdata1$mrp, na.rm = TRUE)+0.01) * 100,
                                     length.out = num_colors + 1),2),
                    domain = mapdata2$mrp * 100)


create_label0 <- function(data) {
  label <- paste0("<strong>Province: </strong>", data$name,
                  "<br><strong>Percentage: </strong>", round(data$mrp * 100, 2), "%")
  return(label)
}


cpp2023<-leaflet(mapdata1_simplified) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~palette(mrp * 100),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              label = ~paste0(round(mrp * 100, 2), "%"),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~create_label0(mapdata2)) %>%
  addLegend(pal = palette,
            values = ~mrp * 100,
            title = "Perceived Priority (%)",
            position = "bottomright",
            labFormat = labelFormat(suffix = "%"))%>%
  addControl(
    html = "<div style='font-size:18px; font-weight:bold;'>Perceived Priority of Climate Change (city-level)</div>",
    position = "bottomleft"
  )
