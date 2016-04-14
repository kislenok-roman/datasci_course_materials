# NOTE: take too long time (may be on my machine)
# we want to set optimal parameters
if (FALSE) {
    a1 <- rbind(train[, list(lat, lon, dt, label, type = "train")],
                validation[, list(lat, lon, dt, label, type = "validation")])

    result <- list()
    for (dist in c(10, 50, 100, 200, 250, 500, 1000)) {
        for (timespan in c("1 week", "1 month", "3 month", "6 month", "1 year", "2 year")) {
            res <- vector("list", nrow(a1))
            for (i in 1:nrow(a1)) {
                res[[i]] <- a1[i,
                               eventsForPoint(all, lat, lon, dt, max.dist = dist, max.timespan = timespan)][, list(label = a1[i, label],
                                                                                                                   type = a1[i, type],
                                                                                                                   value = .N)]
            }
            res <- rbindlist(res)[, label := factor(label, levels = c("Normal", "Dismantle"))]
            g1 <- glm(label ~ value, res[type == "train"], family = "quasibinomial")
            t1 <- table(res[type == "validation", label],
                        factor(predict(g1, newdata = res[type == "validation"], type = "response") >= 0.5, levels = c("FALSE", "TRUE"), labels = c("Normal", "Dismantle")))

            result[[length(result) + 1]] <- list(dist = dist,
                                                 timespan = timespan,
                                                 accuracy = sum(diag(t1)) / sum(t1))
        }
    }
    rbindlist(result)[order(-accuracy)]
}
