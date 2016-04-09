# 1. What tools are you using?
# Are you using any special libraries to work with spatial data?
# How are you visualizing the data?
require(ggplot2)
require(ggmap)

## DRAW A MAP
q1 <- ggplot(data = all[type != "detroit-demolition-permits"], aes(x = lon, y = lat))
# can't use google maps here, I've spent 2500 request while testing...
map <- get_map(location = c(all[, min(lon)], all[, min(lat)], all[, max(lon)], all[, max(lat)]),
               zoom = "auto", scale = 575000, source = "osm")
q1 <- ggmap(map, base_layer = q1, darken = c(0.2, "white"))
q1 <- q1 +
    labs(color = "Density") +
    geom_point(alpha = 0.05, fill = "black", color = "black") +
    ggtitle("Detroit incidents density") +
    geom_point(data = all[type == "detroit-demolition-permits"], aes(x = lon, y = lat),
               color = "red",
               size = 1)

q1

## EVENTS ARE SPREAD ACROSS TIME!!
ggplot(merge(x = all[, list(dt = seq(min(dt), max(dt), by = "1 day")), list(type)],
             y = all[, list(cnt = .N), list(dt, type)],
             by = c("dt", "type"),
             all.x = TRUE)[is.na(cnt), cnt := 0][, list(cnt = sum(cnt), dt = median(dt)), by = list(type, mon = substr(dt, 1, 7))],
       aes(x = dt, y = cnt, color = type)) +
    geom_point(size = 3) +
    geom_line(size = 2) +
    theme_bw() +
    ylab("") +
    xlab("") +
    scale_color_hue("") +
    ggtitle("Event in time")


# 2. In your set of derived buildings, are you noticing any outliers spatially?
# Consider what might be causing these outliers, and how you might specially handle them.
# In data.R I've tried to anser this question with code))

# 3. How did you use the geocoordinates to define a "building"?
# How might you need to change your approach if you were working with 100x the number of incidents?
# How might your solution change if conditions of the problem change?
# Would high-density areas like Manhattan in New York City make differentiation between buildings more difficult?
# Are there heuristics used about how buildings tend to be arranged geometrically that may be violated in certain places?
# None of these issues are necessarily a problem, but the more general the solution, the more likely it is to be used in additional cities.
## I try to write down a function to search dataset for events

# utility function to calculate date in past from `dt`
diff <- function(dt, period) {
    seq(as.Date(dt), by = paste0("-", period), length.out = 2)[2]
}
#
# all - our dataset, should have lat & lon columns
# max.dist - max distance in meters from (lon0, lat0) point, optimaly we will need a decay function, but we can apply it further
# max.timespan - max timespan from dt0, optimally we will need a decay function, but
# lon0, lat0 - base point spacial location
# dt0 - base point time position
eventsForPoint <- function(all, lat0, lon0, dt0, max.dist = 500, max.timespan = "1 year") {
    # degree -> radian
    rad <- function(d) {
        d * pi / 180
    }
    R <- 6371000 # Earth radius
    # filter by date & we need type,
    all[dt >= diff(dt0, max.timespan) & dt < dt0,
                   list(type,
                        code,
                        timespan = dt0 - dt,
                        distance = acos(sin(rad(lat0)) * sin(rad(lat)) + cos(rad(lat0)) * cos(rad(lat)) * cos(rad(lon - lon0))) * R)]
}
decayDistance <- function(meters) {
    1 / meters ^ 2
}
decayTimespan <- function(timespan) {
    1 / timespan
}
