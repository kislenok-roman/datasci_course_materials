require(data.table)
require(stringr)

# process datasets
# 1. detroit-blight-violations
d1 <- fread("data/detroit-blight-violations.csv",
            colClasses = list(character = c(7, 11, 13)), # chars in almost num cols
            verbose = TRUE)

# keep only useful columns
d1 <- d1[, list(id = TicketID,
                type = "detroit-blight-violations",
                address = toupper(paste0(ViolationStreetNumber, " ", ViolationStreetName)),
                dt = paste0(substr(TicketIssuedDT, 7, 10), "-", substr(TicketIssuedDT, 1, 2), "-", substr(TicketIssuedDT, 4, 5), " ", str_pad(TicketIssuedTime, width = 8, side = "left", pad = "0")),
                code = ViolationCode,
                lat = as.numeric(gsub(".*\\n[(]([0-9.\\-]+).*", "\\1", ViolationAddress)),
                lon = as.numeric(gsub(".*, ([0-9.\\-]+)[)]$", "\\1", ViolationAddress)))]

# remove dirty dates
d1 <- d1[dt < '2100' & dt > '2000']
d1 <- d1[,
         list(type,
              address = last(address), # there is duplicates
              dt = last(dt),
              code = last(code),
              lat = last(lat),
              lon = last(lon)),
         by = id]


# 2. detroit-311
d2 <- fread("data/detroit-311.csv")

d2 <- d2
# d2[,  as.Date(paste0(substr(ticket_created_date_time, 7, 10), "-", substr(ticket_created_date_time, 1, 2), "-", substr(ticket_created_date_time, 4, 5)))))]
# 2014-07-14 -- 2015-10-15

# Преступления
d3 <- fread("data/_dcebfb2135a2bf5a6392493bd61aba22_detroit-crime.csv",
            colClasses = list(character = 3),
            verbose = TRUE)

# Разрешние на снос зданий
d4 <- fread("data/_dcebfb2135a2bf5a6392493bd61aba22_detroit-demolition-permits.tsv",
            colClasses = list(character = c(38, 40, 43, 45, 47, 51, 55)))

# ДОБАВОЧКИ
# 1. Local Employment Dynamics OnTheMap (OTM)
# 2. American Community Survey
# 3. The Michigan Liquor Control Commission (MLCC) provided information on the location of
# licensed establishments.
# 4. Finally, the Educational Entity Master file of the Michigan Department of Education provided
# information on the location and types of high schools. For this analysis, a school was considered a
# high school if the grades served included the tenth grade. We measured proximity to a high school
# by a dummy variable indicating whether the census block was within a half mile of a high school.


# ПРЕОБРАЗОВАНИЯ
# http://www.r-bloggers.com/endogenous-spatial-lags-for-the-linear-regression-model/
# Criminal offense data recorded by the Detroit Police Department (DPD) were provided to us at
# point level, with address, latitude and longitude coordinates, offense date, DPD offense category (a
# two-digit code used for summary reporting), and DPD offense code (a four-digit code indicating
# type of offense). Offense codes align directly with the Michigan Incident Crime Reporting (MICR)
# system, allowing for translation from MICR coding to the two main federal coding systems, Uniform
# Crime Reporting (UCR) and the National Incident-Based Reporting System (NIBRS). Both federal
# systems have established methodology for aggregating criminal offenses to meaningful categories,
# which we employ here.
