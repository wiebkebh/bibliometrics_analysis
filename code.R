library(dplyr)
library(jsonlite)
library(httr)

current_date <- Sys.Date()
ten_years_ago <- current_date - 365 * 10
base_url <- "https://api.openalex.org"
page <- 1
has_more_pages <- TRUE
fewer_than_10k_results <- TRUE

construct_api_url <- function(base_url, path, filters = list()) {
  url_components <- c()
  url_components <- c(url_components, base_url, path)

  if (!is.null(filters) && length(filters) > 0) {
    filter_pairs <- sapply(names(filters), function(key) {
      if (is.list(filters[[key]])) {
        paste0(key, ":", paste(filters[[key]], collapse = "|"))
      } else if (is.character(filters[[key]]) && length(filters[[key]]) > 1) {
        paste0(key, ":", paste(filters[[key]], collapse = "|"))
      } else {
        paste0(key, ":", filters[[key]])
      }
    })
    filter_string <- paste(filter_pairs, collapse = ",")
    url_components <- c(url_components, "?filter=", filter_string)
  }
  constructed_url <- paste(url_components, collapse = "")
  return(constructed_url)
}

add_page_parameter <- function(url, page) {
  if (grepl("\\?", url)) {
    return(paste0(url, "&page=", page))
  } else {
    return(paste0(url, "?page=", page))
  }
}

to_df_multi_page <- function(input_url) {
  page <- 1
  has_more_pages <- TRUE
  fewer_than_10k_results <- TRUE
  all_results <- data.frame()

  while (has_more_pages && fewer_than_10k_results) {
    url <- add_page_parameter(input_url, page)
    response <- httr::GET(url)

    if (httr::http_status(response)$category == "Success") {
      json_data <- httr::content(response, "text", encoding = "UTF-8")
      page_results <- jsonlite::fromJSON(json_data, flatten = TRUE)
      new_results <- page_results$results
      reduced_df <- subset(new_results, select = c("id", "language", "primary_location.source.display_name", "type", "publication_date"))
      all_results <- rbind(all_results, reduced_df)
      page <- page + 1

      per_page <- page_results$meta$per_page
      has_more_pages <- length(new_results) >= per_page

      fewer_than_10k_results <- per_page * page <= 10000
    } else {
      print("Error: Failed to retrieve data from the API")
    }
  }
  return(all_results)
}

to_df_single_page <- function(url) {
  response <- httr::GET(url)

  if (httr::http_status(response)$category == "Success") {
    json_data <- httr::content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(json_data, flatten = TRUE)
    results <- data$results
    return(results)
  } else {
    print("Error: Failed to retrieve data from the API")
  }
}

get_inst_ids <- function(url) {
  path_institutions <- "/institutions"
  filters_inst <- list(
    "country_code" = "se",
    "default.search" = c("KTH", "SU", "Karolinska"),
    "type" = "education"
  )
  institutions_url <- construct_api_url(url, path_institutions, filters_inst)
  response_inst <- to_df_single_page(institutions_url)
  return(response_inst)
}

publications_df <- function(url) {
  response_inst <- get_inst_ids(url)
  institution_ids <- gsub('.*/', '', response_inst$id)
  path_works <- "/works"
  filters_works <- list(
    "institutions.id" = institution_ids,
    "from_publication_date" = as.character(ten_years_ago),
    "to_publication_date" = as.character(current_date)
  )
  works_url <- construct_api_url(base_url, path_works, filters_works)
  df <- to_df_multi_page(works_url)
  df$journal <- df$primary_location.source.display_name
  return(df)
}

grouped_works_df <- function(url) {
  df <- publications_df(url)
  grouped_data <- (df %>%
                     group_by(journal)) %>%
    summarise(frequency = n()) %>%
    arrange(desc(frequency))
  return(grouped_data)
}

top_ten_journals <- function(url) {
  grouped_by_journals <- grouped_works_df(url)
  top_ten <- unique(grouped_by_journals$journal)
  head_top_ten <- head(top_ten, 10)
  return(head_top_ten)
}
grouped_publ_df <- grouped_works_df(base_url)
print(str(grouped_publ_df))

