suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(ggmap)) # geocode with google
suppressPackageStartupMessages(require(XML)) # parse State coords

# process datasets
# 1. detroit-blight-violations
loadDetroitBlighViolations <- function(filename = "detroit-blight-violations") {
    d1 <- suppressWarnings(fread(paste0("data/", filename, ".csv"), colClasses = list(character = c(7, 11, 13))))
    # keep only useful columns
    d1 <- d1[, list(id = TicketID,
                    type = "detroit-blight-violations",
                    address = toupper(paste0(ViolationStreetNumber, " ", ViolationStreetName)),
                    dt = paste0(gsub("^([0-9]+)/([0-9]+)/([0-9]+).*", "\\3-\\1-\\2", TicketIssuedDT)),
                    code = ViolationCode,
                    lat = as.numeric(gsub(".*\\n[(]([0-9.\\-]+).*", "\\1", ViolationAddress)),
                    lon = as.numeric(gsub(".*, ([0-9.\\-]+)[)]$", "\\1", ViolationAddress)))]

    # remove dirty dates - it seems that we can find some specific rule to process them, but we would not
    # it'll be not so universal & we really need not too old problems (before 2005)
    d1 <- d1[dt < '2100' & dt > '2000']
    d1 <- d1[,
             list(type,
                  address = last(address), # there are duplicates
                  dt = as.Date(last(dt)),
                  code = last(code),
                  lat = last(lat),
                  lon = last(lon)),
             by = id]
    d1
}

# 2. detroit-311
loadDetroit311 <- function(filename = "detroit-311") {
    d2 <- fread(paste0("data/", filename, ".csv"))

    d2 <- d2[, list(id = ticket_id,
                    type = "detroit-311",
                    address = toupper(gsub("Detroit.*", "", address)),
                    dt = as.Date(gsub("^([0-9]+)/([0-9]+)/([0-9]+).*$", "\\3-\\1-\\2", ticket_created_date_time)),
                    code = issue_type,
                    lat,
                    lon = lng)]
    d2 <- unique(d2)
}
# 3. detroit-crime
loadDetroitCrime <- function(filename = "detroit-crime") {
    d3 <- fread(paste0("data/", filename, ".csv"),
                colClasses = list(character = 3),
                verbose = TRUE)
    # it seems that here we have some events split into multiple row with the same info and
    # diff categories
    d3 <- d3[, list(type = "detroit-crime",
                    address = toupper(ADDRESS[1]),
                    dt = as.Date(gsub("([0-9]+)/([0-9]+)/([0-9]+).*", "\\3-\\1-\\2", INCIDENTDATE[1])),
                    code = paste0(CATEGORY, collapse = ", "),
                    lat = median(LAT),
                    lon = median(LON)),
             by = list(id = CASEID)]
    d3
}

# 4. detroit-demolition-permits
loadDetroitDemolishionPermits <- function(filename = "detroit-demolition-permits") {
    d4 <- fread(paste0("data/", filename, ".tsv"),
                colClasses = list(character = c(38, 40, 43, 45, 47, 51, 55)))

    # there are some warnings about coords
    d4 <- d4[, list(type = "detroit-demolition-permits",
                    address = toupper(SITE_ADDRESS[1]),
                    dt = as.Date(gsub("([0-9]+)/([0-9]+)/([0-9]+)", "20\\3-\\1-\\2", PERMIT_APPLIED[1])),
                    code = "Dismantle",
                    lat = median(as.numeric(gsub(".*[(]([0-9.\\-]+).*", "\\1", site_location))),
                    lon = median(as.numeric(gsub(".*, ([0-9.\\-]+)[)]$", "\\1", site_location)))),
             by = list(id = PERMIT_NO)]

    # it seems that we can expect problems, as data not tidy...
    # d4[!is.na(lat), uniqueN(address), by = list(lat, lon)][V1 > 1]
    # there is (lat, lon) pairs that direct to diff address. I check manualy some addresses... there are problems
    d4
}

# unite
loadDetroitDatasets <- function(doGoogleGeocode = TRUE,
                                blightFilename = "detroit-blight-violations",
                                calls311Filename = "detroit-311",
                                crimeFilename = "detroit-crime") {
    if (!is.null(blightFilename) && !(is.logical(blightFilename) && blightFilename == FALSE)) {
        d1 <- loadDetroitBlighViolations(blightFilename)
    } else {
        d1 <- NULL
    }

    if (!is.null(calls311Filename) && !(is.logical(calls311Filename) && calls311Filename == FALSE)) {
        d2 <- loadDetroit311(calls311Filename)
    } else {
        d2 <- NULL
    }

    if (!is.null(crimeFilename) && !(is.logical(crimeFilename) && crimeFilename == FALSE)) {
        d3 <- loadDetroitCrime(crimeFilename)
    } else {
        d3 <- NULL
    }

    d4 <- loadDetroitDemolishionPermits()
    all <- rbind(d1, d2, d3, d4)
    rm(d1, d2, d3, d4)

    # THERE ARE SOME MISSING/WRONG COORDINATES - we can try to get data from google
    # but for free we can only get 2500 requests per day...
    # or we can try to figure coords by addresses

    # CAUTION! This is city-region-specific. I can't figure out where to get city boundary (or if they even exists)
    # so just drop obviously bad coords for state (MI)
    # coords for states (http://econym.org.uk/gmap/states.xml)
    state_coord_doc <- xmlInternalTreeParse("http://econym.org.uk/gmap/states.xml")
    lats_MI <- as.numeric(xpathSApply(state_coord_doc, "/states/state[@name='Michigan']/point/@lat"))
    lons_MI <- as.numeric(xpathSApply(state_coord_doc, "/states/state[@name='Michigan']/point/@lng"))
    all[lat < min(lats_MI) | lon < min(lons_MI) | lat < min(lats_MI) | lat > max(lats_MI), `:=`(lat = NA, lon = NA)]
    free(state_coord_doc)
    rm(state_coord_doc, lats_MI, lons_MI)

    # but look - there is a lot of outliers even after our "not so precious" clean
    # boxplot(all[, lat])
    all[lat %in% boxplot(all[, lat], plot = FALSE)$out, `:=`(lat = NA, lon = NA)]
    # boxplot(all[, lon])
    all[lon %in% boxplot(all[, lon], plot = FALSE)$out, `:=`(lat = NA, lon = NA)]

    # fix addresses
    all[, address := gsub("^[ ]+|[ ]+$", "", address)] # lead & trail space
    all[, address := gsub("[ ]+", " ", address)] # inner space

    # try to obtain coords by addresses
    miss <- all[is.na(lat) | is.na(lon)]
    all <- all[!is.na(lat) & !is.na(lon)]
    miss <- merge(x = miss[, list(id, type, address, dt, code)],
                  y = all[, list(address, lat, lon)],
                  by = "address",
                  all.x = TRUE)[, list(lat = median(lat),
                                       lon = median(lon)),
                                by = list(id, type, address, dt, code)]

    all <- rbind(all, miss[!is.na(lat) & !is.na(lon)])

    miss <- miss[is.na(lat) | is.na(lon)]

    ## NOTE! It'll take too long time!!!
    if (doGoogleGeocode) {
        # HERE WE FIX city, state & country - can't use universaly
        miss_coord_1 <- miss[, .N, address][order(-N)]
        # google allow to process no more then 2500 addreses
        miss_coord <- geocode(paste0(miss_coord_1[1:pmin(2500, .N), address], " , DETROIT, MI, USA"),
                              source = "google")
        if (nrow(miss_coord_1) > 2500) {
            miss_coord <- rbind(miss_coord,
                                geocode(paste0(miss_coord_1[2501:.N, address], " , DETROIT, MI, USA"),
                                        source = "dsk"))
        }
        miss_coord_1 <- cbind(miss_coord_1, miss_coord)

        miss <- merge(x = miss[, list(id, type, dt, code, address)],
                      y = miss_coord_1[, list(address, lat, lon)],
                      by = "address")

        miss[lat < all[, min(lat)] | lat > all[, max(lat)] | lon < all[, min(lon)] | lon > all[, max(lon)], `:=`(lat = NA, lon = NA)]

        all <- rbind(all, miss[!is.na(lat) & !is.na(lon)])
        rm(miss_coord_1, miss_coord)

        # some incidents still missing coords... silently drop
    }
    rm(miss)

    all
}

if (!file.exists("temp/all-data-with-coords-v1.RData")) {
    all <- loadDetroitDatasets()
    saveRDS(all, "temp/all-data-with-coords-v1.RData")
} else {
    all <- readRDS("temp/all-data-with-coords-v1.RData")
}

## NOTES FOR FUTURE MYSELF
# Additional datasets to consider
# 1. Local Employment Dynamics OnTheMap (OTM)
# 2. American Community Survey
# 3. The Michigan Liquor Control Commission (MLCC) provided information on the location of
# licensed establishments.
# 4. Finally, the Educational Entity Master file of the Michigan Department of Education provided
# information on the location and types of high schools. For this analysis, a school was considered a
# high school if the grades served included the tenth grade. We measured proximity to a high school
# by a dummy variable indicating whether the census block was within a half mile of a high school.
# 5. https://data.detroitmi.gov/browse?category=Property+%26+Parcels&utf8=%E2%9C%93

# TRY TO DO spatial autocorrelation
# http://www.r-bloggers.com/endogenous-spatial-lags-for-the-linear-regression-model/
# Criminal offense data recorded by the Detroit Police Department (DPD) were provided to us at
# point level, with address, latitude and longitude coordinates, offense date, DPD offense category (a
# two-digit code used for summary reporting), and DPD offense code (a four-digit code indicating
# type of offense). Offense codes align directly with the Michigan Incident Crime Reporting (MICR)
# system, allowing for translation from MICR coding to the two main federal coding systems, Uniform
# Crime Reporting (UCR) and the National Incident-Based Reporting System (NIBRS). Both federal
# systems have established methodology for aggregating criminal offenses to meaningful categories,
# which we employ here.
