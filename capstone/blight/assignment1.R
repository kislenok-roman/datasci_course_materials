# 1. What tools are you using?
# Are you using any special libraries to work with spatial data?
# How are you visualizing the data?
require(ggplot2)
require(ggmap)

q1 <- ggplot(data = all[type != "detroit-demolition-permits"], aes(x = lon, y = lat))
map <- get_map(location = c(all[, min(lon)], all[, min(lat)], all[, max(lon)], all[, max(lat)]),
               zoom = "auto", scale = 575000, source = "osm")
q1 <- ggmap(map, base_layer = q1, darken = c(0.2, "white"))
q2 <- q1 +
    labs(color = "Density") +
    geom_point(alpha = 0.05, fill = "black", color = "black") +
    ggtitle("Detroit incidents density") +
    geom_point(data = all[type == "detroit-demolition-permits"], aes(x = lon, y = lat),
               color = "red",
               size = 1)

q2
