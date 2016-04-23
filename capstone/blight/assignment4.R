# For each building, you need to extract a richer set of features from the incident data and construct a
# feature vector. The feature vector will include at least two columns: the building_id and the label.
# You will create many additional columns that the model will use to predict the label.
# These features may be numeric, text-derived, or otherwise computed. Feature engineering is what makes or
# breaks analytics projects, not the method.

# 1. we will rebuild data
source("data.R")
source("assignment1.R")
source("assignment2.R")
source("assignment3.R")
if (!file.exists("temp/all-data-with-coords-v2.RData")) {
    all <- loadDetroitDatasets(calls311Filename = FALSE, # not use calls, we have no needed data in past
                               crimeFilename = "DPD__All_Crime_Incidents__2009_-_Present__Provisional_") # we use more data for crimes (download from Detroit Open Data), original has less data for past
    saveRDS(all, "temp/all-data-with-coords-v2.RData")
} else {
    all <- readRDS("temp/all-data-with-coords-v2.RData")
}

# 2. prepare datasets
dataset <- rbind(train[, list(type = "train", lat, lon, dt, label)],
                 test[, list(type = "test", lat, lon, dt, label)],
                 validation[, list(type = "validation", lat, lon, dt, label)])
rm(train, test, validation, product)

# 3. select features with most predictive power
if (!file.exists("temp/variable.RData")) {
    # run experiment from assignment3 to find several best predictors from the "detroit-blight-violations" dataset
    result <- list()
    for (dstype in c("detroit-blight-violations", "detroit-crime", "detroit-demolition-permits")) {
        for (dist in c(10, 50, 100, 200, 300, 1000)) {
            for (timespan in c("1 week", "1 month", "3 month", "6 month", "1 year", "2 year")) {
                print(paste0(length(result) + 1, ". Going into ", dstype, ", ", dist, "m, ", timespan))

                res <- grabData1(dataset[type %in% c("train", "validation")],
                                 all[type == dstype],
                                 dist,
                                 timespan)
                g1 <- glm(label ~ value, res[type == "train"], family = "quasibinomial")

                t1 <- table(real = res[type == "validation", label],
                            pred = factor(predict(g1,
                                                  newdata = res[type == "validation"],
                                                  type = "response") >= 0.5,
                                          levels = c("FALSE", "TRUE"),
                                          labels = c("Normal", "Dismantle")))
                tp <- t1[2, 2]
                tn <- t1[2, 1]
                fp <- t1[1, 2]
                fn <- t1[2, 2]

                result[[length(result) + 1]] <- list(type = dstype,
                                                     dist = dist,
                                                     timespan = timespan,
                                                     sensitivity = tp / sum(t1[2, ]),
                                                     precision = tp / sum(t1[, 2]),
                                                     accuracy = sum(diag(t1)) / sum(t1),
                                                     f1 = 2 * tp / (2 * tp + fp + fn))
            }
        }
    }
    rm(res, tn, tp, fn, fp, t1, g1)
    rbindlist(result)[sensitivity >= 0.5 | precision > 0.5][order(-accuracy, -f1)]
    # select the best predictors
    # detroit-blight-violations
    #  300m + 1 month
    #  10m + 6 month

    # detroit-crime:
    #  1000m + 1 month
    #  50m + 1 year

    # detroit-demolition-permits
    #  1000m + 1 month
    choosen_predictors <- list(list(dstype = "detroit-blight-violations", dist = 300, timespan = "1 month"),
                               list(dstype = "detroit-blight-violations", dist = 10, timespan = "6 month"),
                               list(dstype = "detroit-crime", dist = 1000, timespan = "1 month"),
                               list(dstype = "detroit-crime", dist = 50, timespan = "1 year"),
                               list(dstype = "detroit-demolition-permits", dist = 1000, timespan = "1 month"))
    saveRDS(choosen_predictors, "temp/variable.RData")
} else {
    choosen_predictors <- readRDS("temp/variable.RData")
}

# 4. produce result dataset
if (!file.exists("temp/result_dataset.RData")) {
    dataset_features <- lapply(1:length(choosen_predictors), function(i) {
        res <- grabData1(dataset,
                         all[type == choosen_predictors[[i]]$dstype],
                         choosen_predictors[[i]]$dist,
                         choosen_predictors[[i]]$timespan)

        value_name <- paste0(choosen_predictors[[i]], collapse = "_")
        if (i != 1) {
            # we'll keep label only in a first grab
            res[, `:=`(label = NULL, type = NULL)]
        }
        setnames(res, "value", value_name)
    })
    dataset_features <- do.call("cbind", dataset_features)

    saveRDS(dataset_features, "temp/result_dataset.RData")
} else {
    dataset_features <- readRDS("temp/result_dataset.RData")
}
