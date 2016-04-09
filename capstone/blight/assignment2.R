round5 <- function(x) {
    floor(20000 * x) / 20000
}
all[, `:=`(lat = round5(lat), lon = round5(lon))]
# we need success (Demolished)
train <- all[type == "detroit-demolition-permits", list(dt = min(dt), label = "Dismantle"), list(lat, lon)]
# and good examples
set.seed(20160408)
# we get data from same period!
# not contained in detroit-demolishion-permits
other1 <- merge(x = all[type != "detroit-demolition-permits" &
                        dt >= train[, min(dt)] &
                        dt <= train[, max(dt)]],
                y = train[, list(lat, lon, label)],
                by = c("lat", "lon"),
                all.x = TRUE)[is.na(label), list(dt = min(dt), label = "Normal"), list(lat, lon)]
train <-  rbind(train,
                other1[sample.int(nrow(other1), nrow(train))])
rm(other1)

# look at data, looks ok
q1 <- ggplot(data = train, aes(x = lon, y = lat))
map <- get_map(location = c(all[, min(lon)], all[, min(lat)], all[, max(lon)], all[, max(lat)]),
               zoom = "auto", scale = 575000, source = "osm")
q1 <- ggmap(map, base_layer = q1, darken = c(0.2, "white"))
q1 <- q1 +
    labs(color = "Train") +
    geom_point(aes(color = label)) +
    ggtitle("Detroit incidents density")
q1

# really I want to get test & validation datasets, as there are no validation/test
size <- 0.1 * nrow(train)
s1 <- sample.int(nrow(train), size)
test <- train[s1]
train <- train[-s1]
s1 <- sample.int(nrow(train), size)
validation <- train[s1]
train <- train[-s1]


