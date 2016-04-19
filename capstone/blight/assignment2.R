round5 <- function(x) {
    floor(20000 * x) / 20000
}
all[, `:=`(lat = round5(lat), lon = round5(lon))]
# we need success (Demolished)
train <- all[type == "detroit-demolition-permits", list(dt = min(dt), label = "Dismantle"), list(lat, lon)]
# and good examples
# we get data from same period!
# not contained in detroit-demolishion-permits
product <- merge(x = all[type != "detroit-demolition-permits" &
                             dt >= train[, min(dt)] &
                             dt <= train[, max(dt)]],
                 y = train[, list(lat, lon, label)],
                 by = c("lat", "lon"),
                 all.x = TRUE)[is.na(label),
                               list(dt = all[type == "detroit-demolition-permits", max(dt)], # we know that Normal buildings still here), but we have no super fresh data
                                    label = "Normal"),
                               list(lat, lon)]
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

# really I want to get test & validation datasets, as there are no validation/test provided
# we can't use train dataset to evaluate our result - so we need test dataset for that reason.
# also we'll need some dataset to evaluate model parameters - validation
# our result to publish (in assignment6) will be based on product dataset - all points without labels currently
# Based on the ideas of the Matthew Dixon answer for this assighment
# the idea here is to have some Success in a test/validation, but no more than in
# a real dataset
success_ratio <- round(nrow(train) / nrow(product), 2) # around 5%, may be it's still too large
# 90% of Dismantled observations - train     : 50% Dismantled + 50% Normal
# 5% of Dismantled observations  - test      : 5% Dismabtled + 95% Normal
# 5% of Dismantled observations  - vaidation : 5% Dismabtled + 95% Normal
set.seed(20160408)
product <- product[order(sample.int(.N))] # we'll access it sequantially so need to resample it as a whole
train <- train[order(sample.int(.N))]
test_size <- train[, ceiling(0.05 * .N)]
test <- rbind(train[1:test_size], # 5% Dismantled
              product[1:ceiling(test_size / 0.05)]) # + 95% Normal
validation <- rbind(train[test_size + 1:test_size], # 5% Dismantled
                    product[ceiling(test_size / 0.05) + 1:ceiling(test_size / 0.05)]) # + 95% Normal
train <-  rbind(train[2 * test_size + 1:(.N - 2 * test_size)], # 80% Dismantled
                product[2 * ceiling(test_size / 0.05) + 1:(nrow(train) - 2 * test_size)]) # + eq Normal

# test for eq in success/non-success examples
train[, .N, by = label][, list(label, N, prob = N / sum(N))]
test[, .N, by = label][, list(label, N, prob = N / sum(N))]
validation[, .N, by = label][, list(label, N, prob = N / sum(N))]

## 1. Why is it in important to ignore the 'Dismantle' incidents during evaluation?
# We will evaluate on test dataset using both classified buildings.
# I don't know why we should ignore the 'Dismantle' events. There are no reasons I think.
# Also we'll use product dataset (where are only Normal buildings to the time) to predict probability of Dismantle vs. Normal
# As there are relatively small amount of Dismantled buildings we should choose to maximiza Sensitivity

## 2. Could our labels be incorrect?
# How might you validate that our labeling scheme accurately reflected ground truth?
# Giving other problems with this dataset there are no reasons to think that any labeling scheme can reflect ground truth.
# I think we can trust for Dismantled buildings as city pay to contractor and (I want to belive) check the result.
# Also we can test for abscence of future events for a dismantled buildings (somebody called for help) as a proxy.

## 3. How many blighted buildings did you come up with?
dcast.data.table(rbind(train[, list(label, group = "1. train")],
                       validation[, list(label, group = "2. validation")],
                       test[, list(label, group = "3. test")]),
                 label ~ group,
                 fun.aggregate = length,
                 value.var = "group")[, list(label,
                                             `1. train`,
                                             `2. validation`,
                                             `3. test`,
                                             all = `1. train` + `2. validation` + `3. test`)]
