#' Get works from Google Scholar
#'
#' @details
#' R package `scholar` is used to retrieve works from Google Scholar according to `google-scholar`
#' field in Tiddlywiki. As Google Scholar does not provide DOI for publications, the publication
#' is matched according to the same published year and journal and similar title. Consequently,
#' the matching might not be accurate.
#'
#' The retrieved works are stored into subfolder `scholar` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from Google Scholar
#' @export
works_scholar <- function(is_new = FALSE) {

    f <- "[tag[Colleague]has[google-scholar]!has[draft.of]]"

    scholar_ids <- rtiddlywiki::get_tiddlers(f)
    if (length(scholar_ids) == 0) {
        return(NULL)
    }
    scholar_ids <- scholar_ids |>
        purrr::map_df(function(x){
            list(title = x$title,
                           scholar = x$`google-scholar`)
        }) |>
        dplyr::mutate(scholar = gsub("^.*user=([a-zA-Z0-9_-]+).*$", "\\1", stringi::stri_trim(.data$scholar)))

    out_folder <- file.path(tws_options()$output, "scholar")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", scholar_ids$scholar))
        remove_outfiles(files = files)
    }

    request_num <- tws_options()$author_max
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(scholar_ids))) {
        out_file <- file.path(out_folder, paste0(scholar_ids$scholar[i], ".Rds"))
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
            message("Get works from Google Scholar for ", scholar_ids$title[i])


            x <- try({
                authors <- scholar::get_publications(scholar_ids$scholar[i])
                Sys.sleep(stats::runif(1) * 10 + 10)
                if (is.null(authors)) {
                    stop("Empty authors")
                }
                if (nrow(authors) == 0) {
                    next
                }
                saveRDS(authors, out_file)
                authors$is_new <- TRUE
            })
            if (inherits(x, "try-error")) {
                next
            }
        }
        works$scholar <-  scholar_ids$scholar[i]
        all_works[[i]] <- works
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()


    if (is_new) {
        all_works <- all_works |>
            dplyr::filter(is_new)
    }

    if (nrow(all_works) == 0) {
        message("No new authors")
        return(invisible())
    }

    pubs <- all_works  |>
        dplyr::filter(!is.na(.data$cid))  |>
        dplyr::group_by(.data$cid, .data$pubid) |>
        dplyr::filter(!is.na(.data$year))  |>
        dplyr::slice(1) |>
        dplyr::mutate(title = tolower(.data$title)) |>
        dplyr::arrange(.data$title) |>
        dplyr::mutate(year = as.character(.data$year),
               journal = tolower(.data$journal))

    # Match bibtex-entry with similarity
    tiddllers <- rtiddlywiki::get_tiddlers(filter = "[tag[bibtex-entry]has[bibtex-doi]has[bibtex-journaltitle]has[bibtex-year]has[bibtex-title]]")

    i <- 1
    par_get <- function(i, tiddllers, pubs) {
        a <- tiddllers[[i]]
        pubs_i <- pubs[a$`bibtex-year` == pubs$year &
                           pubs$journal == tolower(a$`bibtex-journaltitle`),]
        sim_i <- stringdist::stringsim(pubs_i$title, a$`bibtex-title`)
        filter1 <- pubs_i[sim_i > 0.7,]
        if (nrow(filter1) >= 1 && nrow(filter1) < 40) {
            filter1$doi <- a$`bibtex-doi`
            filter1$key <- a$title
            return(filter1)
        }
        return(NULL)
    }
    system.time({
        res <- NULL
        for (i in seq(along = tiddllers)) {
            res_i <- par_get(i, tiddllers, pubs)
            res[[i]] <- res_i
        }
    })
    res <- dplyr::bind_rows(res)

    works_ids <- all_works |>
        dplyr::select("pubid", "scholar") |>
        dplyr::distinct()
    res2 <- res |>
        dplyr::ungroup() |>
        dplyr::mutate(doi = gsub("https?://.*doi\\.org/", "", .data$doi)) |>
        dplyr::select("pubid", "scholar", "doi", "key") |>
        dplyr::distinct() |>
        dplyr::left_join(scholar_ids, by = "scholar") |>
        dplyr::select("doi", "title")
    works_authoring(res2, is_new)
    return(invisible())
}
