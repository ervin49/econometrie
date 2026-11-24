# Econometrics project - Week 1
# Student: Ervin Marin | Group: 1099
# Research question: Across European countries in 2023, are tertiary education,
# the overall employment rate, and the early-leaver rate associated with youth unemployment?
# Four continuous variables, no GDP variable, and no dummy variables.
# Eurostat data are downloaded when this script is run in RStudio with internet access.

# Uses jsonlite to read Eurostat's JSON-stat API response. It has no Fortran dependency.
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

reference_year <- 2023
# Broad European country sample. After merging, only countries with complete data
# on all four variables are retained. The script checks the minimum n = 30 rule.
country_codes <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE", "IS", "LI", "NO",
  "CH", "UK", "AL", "BA", "ME", "MK", "RS", "TR"
)

get_one_year <- function(dataset, filters, output_name) {
  # Eurostat Statistics API: filters are dimension names, and the response is JSON-stat.
  params <- c(
    unlist(lapply(names(filters), function(nm) {
      paste0(URLencode(nm, reserved=TRUE), "=", URLencode(as.character(filters[[nm]]), reserved=TRUE))
    })),
    paste0("geo=", country_codes),
    paste0("sinceTimePeriod=", reference_year),
    paste0("untilTimePeriod=", reference_year),
    "format=JSON", "lang=en"
  )
  url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
                dataset, "?", paste(params, collapse="&"))
  temp_file <- tempfile(fileext=".json")
  status <- tryCatch(
    download.file(url, temp_file, mode="wb", quiet=TRUE),
    error=function(e) stop(paste("Could not download Eurostat dataset", dataset, "-", conditionMessage(e)))
  )
  if (!identical(status, 0L)) stop(paste("Download failed for Eurostat dataset", dataset))
  js <- tryCatch(
    jsonlite::fromJSON(temp_file, simplifyVector=FALSE),
    error=function(e) stop(paste("Could not parse Eurostat response for", dataset, "-", conditionMessage(e)))
  )
  unlink(temp_file)
  if (is.null(js$id) || is.null(js$size) || is.null(js$dimension) || is.null(js$value)) {
    stop(paste("Eurostat returned no observations for", dataset))
  }

  ids <- unlist(js$id, use.names=FALSE)
  sizes <- as.integer(unlist(js$size, use.names=FALSE))
  get_position <- function(dim_name, code) {
    ix <- js$dimension[[dim_name]]$category$index
    if (is.null(ix)) stop(paste("Missing dimension index", dim_name, "in", dataset))
    position <- ix[[code]]
    if (is.null(position)) return(NA_integer_)
    as.integer(position)
  }
  geo_dim <- match("geo", ids)
  time_dim <- match("time", ids)
  if (is.na(geo_dim) || is.na(time_dim)) stop(paste("Expected geo/time dimensions missing in", dataset))
  weights <- if (length(sizes) == 1L) 1 else c(rev(cumprod(rev(sizes[-1L]))), 1)
  values <- unlist(js$value, use.names=TRUE)

  result <- lapply(country_codes, function(country) {
    positions <- rep(0L, length(ids))
    geo_position <- get_position("geo", country)
    time_position <- get_position("time", as.character(reference_year))
    if (is.na(geo_position) || is.na(time_position)) {
      return(data.frame(geo=country, values=NA_real_, stringsAsFactors=FALSE))
    }
    positions[geo_dim] <- geo_position
    positions[time_dim] <- time_position
    flat_index <- sum(positions * weights)
    value <- values[as.character(flat_index)]
    if (!length(value)) value <- NA_real_ else value <- as.numeric(value[[1L]])
    data.frame(geo=country, values=value, stringsAsFactors=FALSE)
  })
  out <- do.call(rbind, result)
  names(out)[2] <- output_name
  out
}

# Y: youth unemployment rate, ages 15-24, both sexes, percent of labour force.
y <- get_one_year("une_rt_a", list(freq="A", unit="PC_ACT", age="Y15-24", sex="T"), "youth_unemployment_pct")
# X1: tertiary attainment, ages 25-34, both sexes, percentage of population.
x1 <- get_one_year("edat_lfse_03", list(freq="A", unit="PC", age="Y25-34", sex="T", isced11="ED5-8"), "tertiary_education_pct")
# X2: employment rate, ages 20-64, both sexes, percentage of population.
x2 <- get_one_year("lfsi_emp_a", list(freq="A", indic_em="EMP_LFS", age="Y20-64", unit="PC_POP", sex="T"), "employment_rate_20_64_pct")
# X3: early leavers from education and training, ages 18-24, percentage of population.
x3 <- get_one_year("edat_lfse_14", list(freq="A", wstatus="POP", unit="PC", age="Y18-24", sex="T"), "early_leavers_18_24_pct")

# Merge by country identifier and keep only complete observations. Do not replace
# missing data with zero. The same complete sample will be used for both models.
dat <- Reduce(function(a, b) merge(a, b, by="geo", all=TRUE), list(y, x1, x2, x3))
dat <- dat[!duplicated(dat$geo), ]
all_vars <- c("youth_unemployment_pct", "tertiary_education_pct", "employment_rate_20_64_pct", "early_leavers_18_24_pct")
complete <- complete.cases(dat[, all_vars])
excluded <- dat[!complete, "geo"]
analysis_data <- dat[complete, c("geo", all_vars)]
country_labels <- c(
  AT="Austria", BA="Bosnia and Herzegovina", BE="Belgium", BG="Bulgaria",
  CH="Switzerland", CY="Cyprus", CZ="Czechia", DE="Germany", DK="Denmark",
  EE="Estonia", EL="Greece", ES="Spain", FI="Finland", FR="France",
  HR="Croatia", HU="Hungary", IE="Ireland", IS="Iceland", IT="Italy",
  LT="Lithuania", LU="Luxembourg", LV="Latvia", ME="Montenegro", MK="North Macedonia",
  MT="Malta", NL="Netherlands", NO="Norway", PL="Poland", PT="Portugal",
  RO="Romania", RS="Serbia", SE="Sweden", SI="Slovenia", SK="Slovakia",
  TR="Türkiye", UK="United Kingdom"
)
analysis_data$country_name <- unname(country_labels[analysis_data$geo])
analysis_data <- analysis_data[, c("geo", "country_name", all_vars)]

cat("Reference year:", reference_year, "\n")
cat("Countries retrieved before complete-case selection:", nrow(dat), "\n")
cat("Complete observations retained:", nrow(analysis_data), "\n")
cat("Countries excluded due to missing values:", if(length(excluded)) paste(excluded, collapse=", ") else "none", "\n")
cat("Duplicate country identifiers after merge:", sum(duplicated(analysis_data$geo)), "\n")
str(analysis_data)

if (nrow(analysis_data) < 30) {
  stop("Fewer than 30 complete countries. Expand the country sample or choose a year with the lecturer before proceeding.")
}
if (anyDuplicated(analysis_data$geo)) stop("Duplicate country rows detected.")

write.csv(analysis_data, "Ervin_Marin_Week1.csv", row.names=FALSE, na="")
cat("Saved Ervin_Marin_Week1.csv\n")
