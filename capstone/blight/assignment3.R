# simple attempt to gather data
# a1 - dataset of buildings
# all - data for al events considered
# max.dist - max distance from buildings to events being considered
# max.timespan - max timespan from time we know label for building to events to be considered
grabData1 <- function(a1, all, max.dist, max.timespan) {
    # filter only to interested data
    a2 <- all[dt >= a1[, min(dt)] %diff% max.timespan & dt <= a1[, max(dt)]]

    if (nrow(a2) > 0) {
        a2[, p := 1] # need cartesian join
        a1 <- a1[, list(p = 1, lat0 = lat, lon0 = lon, dt0 = dt, label, type0 = type)] # data.table do ops by reference and we want to keep original a1, so do "copy"
        setkey(a1, p)
        setkey(a2, p)

        res <- a2[a1, {
            q1 <- dt >= dt0[1] %diff% max.timespan & dt < dt0[1] # in a period
            if (any(q1)) {
                q2 <- distanceEarth(lat0[1], lon0[1], lat[q1], lon[q1]) <= max.dist # in a Radius
                list(label = label[1], type = type0[1], value = sum(q1[q1][q2]))
            } else {
                list(label = label[1], type = type0[1], value = 0L)
            }
        }, by = .EACHI][, p := NULL]
    } else {
        res <- a1[, list(label, type, value = 0L)]
    }
    res[, label := factor(label, levels = c("Normal", "Dismantle"))]
}

# NOTE: take too long time (may be on my machine)
# we want to set optimal parameters (distance & timespan)
if (FALSE) {
    a1 <- rbind(train[, list(lat, lon, dt, label, type = "train")],
                validation[, list(lat, lon, dt, label, type = "validation")])

    result <- list()
    for (dist in c(10, 20, 40, 50, 60, 80, 100, 250, 200, 300)) {
        for (timespan in c("1 week", "1 month", "3 month", "6 month", "1 year", "2 year")) {
            res <- grabData1(a1, all[type == "detroit-blight-violations"], dist, timespan)
            g1 <- glm(label ~ value, res[type == "train"], family = "quasibinomial")

            t1 <- table(real = res[type == "validation", label],
                        pred = factor(predict(g1,
                                              newdata = res[type == "validation"],
                                              type = "response") >= 0.5,
                                      levels = c("FALSE", "TRUE"),
                                      labels = c("Normal", "Dismantle")))
            tp <- t1[1, 1]
            tn <- t1[2, 2]
            fp <- t1[2, 1]
            fn <- t1[2, 2]

            result[[length(result) + 1]] <- list(dist = dist,
                                                 timespan = timespan,
                                                 sensitivity = tp / sum(t1[1, ]),
                                                 precision = tp / sum(t1[, 1]),
                                                 accuracy = sum(diag(t1)) / sum(t1),
                                                 f1 = 2 * tp / (2 * tp + fp + fn))
        }
    }
    # choose our model specification
    rbindlist(result)[order(-accuracy)]
    # max accuracy is 0.63 for 300m & 1 month
}

# fit a model
a1 <- rbind(train[, list(lat, lon, dt, label, type = "train")],
            validation[, list(lat, lon, dt, label, type = "validation")],
            test[, list(lat, lon, dt, label, type = "test")])
res <- grabData1(a1, all[type == "detroit-blight-violations"], 300, "1 month")
g1 <- glm(label ~ value, res[type %in% c("train", "validation")], family = "quasibinomial")
summary(g1)

# validate
res[type == "test", list(label,
                         labelPredict = factor(predict(g1,
                                                       newdata = res[type == "test"],
                                                       type = "response") >= 0.5,
                                               levels = c("FALSE", "TRUE"),
                                               labels = c("Normal", "Dismantle")))][, list(sum(label == labelPredict) / .N)]
# 0.65
