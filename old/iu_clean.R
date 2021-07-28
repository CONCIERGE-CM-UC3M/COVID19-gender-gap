source(knitr::purl("exploratory/ae_load.Rmd", output=tempfile()))
rm(conn_table_CONCIERGE)
library(data.table)

local(for (name in ls(envir=.GlobalEnv)) {
  df <- as.data.table(get(name))
  df <- df[!duplicated(df)]
  assign(name, df, .GlobalEnv)
})

articles <- merge(categories, articles)
rm(categories)
articles <- articles[repository != "arxiv" | !is.na(category)]
articles[, subcategory := subcategory_name]
articles <- articles[, c("id", "category", "subcategory", "repository", "date")]
articles[subcategory %in% c("Epidemiology", "Clinical Trials"), repository := "medrxiv"]
articles[repository == "medrxiv", category := "Health Sciences"]
articles[repository == "biorxiv", category := "Biology"]
articles[repository == "psyarxiv", category := "Psychology"]
articles[repository == "socarxiv", category := "Social Sciences"]
articles[category == "Quantitative Biology", category := "Biology"]
articles[category == "Quantitative Finance", category := "Economics"]
articles <- articles[subcategory != "Social and Behavioral Sciences"]
################################################################################
# socarxiv adjustments
articles[repository == "socarxiv", subcategory := sub(
  paste0(
    "Science and Technology Studies|",
    "Environmental Studies|",
    "International and Area Studies|",
    "Leisure Studies|",
    "Organization Development|",
    "Leadership Studies"
  ), "Other Social and Behavioral Sciences", subcategory
)]
articles[repository == "socarxiv", subcategory := sub(
  paste0(
    "Library and Information Science|",
    "Linguistics"
  ), "Arts and Humanities", subcategory
)]
articles[repository == "socarxiv", subcategory := sub(
  paste0(
    "Urban Studies and Planning|",
    "Social Statistics"
  ), "Sociology", subcategory
)]
articles[repository == "socarxiv", subcategory := sub(
  paste0(
    "Social Work|",
    "Legal Studies"
  ), "Law", subcategory
)]
articles[repository == "socarxiv", subcategory := sub(
  "Public Affairs, Public Policy and Public Administration",
  "Political Science", subcategory
)]
articles[repository == "socarxiv", subcategory := sub(
  "Agricultural and Resource Economics",
  "Economics", subcategory
)]
# move stuff to psychology
psyc <- unique(articles[repository == "socarxiv" & subcategory == "Psychology"]$id)
articles[repository == "socarxiv" & id %in% psyc, category := "Psychology"]
rm(psyc)
################################################################################
articles <- articles[subcategory != "Psychology"]
articles <- na.omit(articles)
articles <- articles[!duplicated(articles)]
################################################################################
# restrict socarxiv and psyarxiv to main subcategories
get_main <- function(repo, data, n=100) {
  data <- data[repository %in% repo]
  w <- data[, .(weight=.N), by=subcategory][order(weight, decreasing=TRUE)]
  data[, .(main = w$subcategory[w$subcategory %in% subcategory][1]), by=id][
    , .N, by=main][order(N, decreasing=TRUE)][N >= n]$main
}
main <- unlist(lapply(c("socarxiv", "psyarxiv"), get_main, articles))
articles <- articles[(!repository %in% c("socarxiv", "psyarxiv")) | subcategory %in% main]
rm(main, get_main)
################################################################################

# checks
articles[, .(N=length(unique(subcategory))), by=category][order(category)]
articles[, .(N=length(unique(id))), by=category][order(category)]
