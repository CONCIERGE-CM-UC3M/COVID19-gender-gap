source("iu_clean.R")

# covid, not covid
text[, covidpaper := grepl("covid-19", title, ignore.case=TRUE) |
       grepl("sars-cov-2", title, ignore.case=TRUE) |
       grepl("coronavirus", title, ignore.case=TRUE)]

merge_info <- function(data) {
  df <- merge(data, articles, by="id")
  df <- merge(df, text[, c("id", "covidpaper")], by="id")
  df <- df[!duplicated(df)]
  df <- na.omit(df)
  df[, `:=`(
    year = lubridate::year(date),
    month = lubridate::month(date),
    week = lubridate::week(date)
  )]
  df[year < 2020, covidpaper := FALSE]
  # remove subcategories without past data
  df <- df[, minyear := min(year)][minyear < 2020][, minyear := NULL]
  df
}

# features all
df <- merge_info(authors[, .(
  n_male = sum(gender == "male", na.rm=TRUE),
  n_female = sum(gender == "female", na.rm=TRUE),
  n_na = sum(is.na(gender))
), by=id])

# features first
df.first <- merge_info(authors[alphabetical_ordered==FALSE & rank=="first"])

# features last
df.last <- merge_info(authors[alphabetical_ordered==FALSE & rank=="last"])
