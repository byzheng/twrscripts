
wos_starter_request <- function(query, page, limit = 50) {
    key <- Sys.getenv("WOS_STARTER_KEY")
    stopifnot(nchar(key) > 0)
    base <- "https://api.clarivate.com/apis/wos-starter/v1/documents"
    response <- httr::GET(base,
                          query = list(q=query, limit = limit, page = page),
                          config = httr::add_headers("X-ApiKey" = key))
    Sys.sleep(stats::runif(1) * 3 + 1)
    #message(httr::status_code(response))
    stopifnot(httr::status_code(response) == 200)
    httr::content(response)
}

wos_works_get <- function(query) {
    works <- wos_starter_request(query = query, page = 1, limit = 50)
    works_total <- works$metadata$total
    pages_total <- ceiling(works_total / 50)
    all_hits <- works$hits
    if (pages_total > 1) {
        i <- 1
        for (i in seq_len(pages_total - 1)) {
            works_i <- wos_starter_request(query = query, page = i + 1, limit = 50)
            all_hits <- c(all_hits, works_i$hits)
        }
    }
    all_hits
}



#' Get works from ResearchID using ResearchID API.
#'
#' @details
#' [WOS Starter API] (https://developer.clarivate.com/apis/wos-starter) is used to retrieve works
#' from Web of Scient according to `researcherid` field in Tiddlywiki.
#' This package use environment variable to authenticate WOS (i.e. `WOS_STARTER_KEY`).
#'
#' The retrieved works are stored into subfolder `researcherid` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from WOS
#' @export
works_wos <- function(is_new = FALSE) {

    f <- "[tag[Colleague]has[researcherid]!has[draft.of]]"

    wos_ids <- rtiddlywiki::get_tiddlers(f)
    if (length(wos_ids) == 0) {
        return(NULL)
    }
    wos_ids <- wos_ids |>
        purrr::map_df(function(x){
            list(title = x$title,
                           wos = x$researcherid)
        }) |>
        dplyr::mutate(wos = basename(.data$wos))

    out_folder <- file.path(tws_options()$output, "researcherid")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", wos_ids$wos))
        remove_outfiles(files = files)
    }

    request_num <- tws_options()$author_max
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(wos_ids))) {
        out_file <- file.path(out_folder, paste0(wos_ids$wos[i], ".Rds"))
        if (file.exists(out_file)) {
            works <- readRDS(out_file) |>
                dplyr::mutate(is_new = FALSE)
        } else {
            request_num_now <- request_num_now + 1
            if (request_num_now > request_num) {
                message("Request number is more than the limit ", request_num)
                message("Call this function again for more request")
                break
            }
            message("Get works from Web of Science for ", wos_ids$title[i])


            x <- try({
                all_hits <- wos_works_get(query = sprintf("AI=%s", wos_ids$wos[i]))
                dois <- all_hits |>
                    purrr::map(function(x) {
                        if (is.null(x$identifiers$doi)) {
                            return(NULL)
                        }
                        x$identifiers$doi
                    }) |>
                    unlist()  |>
                    unique()
                works <- tibble::tibble(title = wos_ids$title[i], doi = dois)
                works$is_new <- TRUE
                saveRDS(works, out_file)
            })
            if (inherits(x, "try-error")) {
                next
            }
            Sys.sleep(1)
        }
        all_works[[i]] <- works
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()

    works_authoring(all_works, is_new)
}
