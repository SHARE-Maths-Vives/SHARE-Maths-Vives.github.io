library(httr)
library(jsonlite)

# Référence du projet ANR
anr_reference <- "ANR-24-EXMA-0008"

# Requête HAL
res <- GET(
  "https://api.hal.science/search/",
  query = list(
    q = paste0("anrProjectReference_s:", anr_reference),
    wt = "json",
    rows = 500,
    sort = "producedDateY_i desc",
    fl = paste(
      "title_s",
      "authFullName_s",
      "producedDateY_i",
      "journalTitle_s",
      "doiId_s",
      "uri_s",
      "docType_s",
      "conferenceTitle_s",
      "bookTitle_s",
      sep = ","
    )
  )
)

stop_for_status(res)

# Lecture des résultats
data <- fromJSON(
  content(res, "text", encoding = "UTF-8"),
  simplifyVector = FALSE
)

docs <- data$response$docs

# Fonction d'extraction
get_field <- function(doc, field) {
  
  x <- doc[[field]]
  
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  
  paste(as.character(x), collapse = "; ")
}

# Extraction des publications
publications <- lapply(docs, function(doc) {
  
  list(
    title = get_field(doc, "title_s"),
    authors = get_field(doc, "authFullName_s"),
    year = get_field(doc, "producedDateY_i"),
    journal = get_field(doc, "journalTitle_s"),
    doi = get_field(doc, "doiId_s"),
    hal_url = get_field(doc, "uri_s"),
    type = get_field(doc, "docType_s"),
    conference = get_field(doc, "conferenceTitle_s"),
    book = get_field(doc, "bookTitle_s")
  )
  
})

# Création du dossier data
if (!dir.exists("data")) {
  dir.create("data")
}

# Export JSON
write_json(
  publications,
  "data/publications.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

message(length(publications), " publications récupérées depuis HAL.")