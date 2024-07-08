
#' Get works from Scopus using Scopus API.
#'
#' @details
#' R package `rscopus` is used to retrieve works from Scopus according to `scopus`
#' field in Tiddlywiki.
#'
#' The retrieved works are stored into subfolder `scopus` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from Scopus
#' @export
works_scopus <- function(is_new = FALSE) {
    rtiddlywiki::tw_options(host = tws_options()$host)

    f <- "[tag[Colleague]has[scopus]!has[draft.of]]"

    scopus_ids <- rtiddlywiki::get_tiddlers(f)
    if (length(scopus_ids) == 0) {
        return(NULL)
    }

    get_authorid <- function(url) {
        res <- c()
        for (i in seq(along = url)) {
            res[i] <- httr2::url_parse(url[i])$query$authorId
        }
        res
    }
    scopus_ids <- scopus_ids |>
        purrr::map_df(function(x){
            tibble::tibble(title = x$title,
                   scopus = x$scopus)
        }) |>
        dplyr::mutate(scopus = get_authorid(.data$scopus))

    out_folder <- file.path(tws_options()$output, "scopus")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", scopus_ids$scopus))
        remove_outfiles(files = files)
    }

    request_num <- tws_options()$author_max
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(scopus_ids))) {
        out_file <- file.path(out_folder, paste0(scopus_ids$scopus[i], ".Rds"))
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
            message("Get works from SCOPUS for ", scopus_ids$title[i])
            works <- rscopus::author_df(au_id = scopus_ids$scopus[i]) |>
                dplyr::mutate(is_new = TRUE)
            saveRDS(works, out_file)
            Sys.sleep(1)
        }
        works <- works |>
            dplyr::select(doi = "prism:doi", is_new) |>
            dplyr::mutate(title = scopus_ids$title[i])
        all_works[[i]] <- works
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()

    works_authoring(all_works, is_new)

    return(invisible())
}
