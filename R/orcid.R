
#' Get works from ORCID using ORCID API.
#'
#' @details
#' R package `rorcid` is used to retrieve works from ORCID according to `orcid`
#' field in Tiddlywiki. Package `rorcid` is archived now and there is no alternative
#' R package to access ORCID API in R. This package use two system variables to
#' authenticate orcid (i.e. `ORCID_CLIENT_ID` and `ORCID_CLIENT_SECRET`).
#'
#' The retrieved works are stored into subfolder `orcid` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from ORCID
#' @export
works_orcid <- function(is_new = FALSE) {

    rorcid::orcid_auth(scope = "/read-public",
                   client_id = Sys.getenv("ORCID_CLIENT_ID"),
                   client_secret = Sys.getenv("ORCID_CLIENT_SECRET"))
    f <- "[tag[Colleague]has[orcid]!has[draft.of]]"

    orcid_ids <- rtiddlywiki::get_tiddlers(f)
    if (length(orcid_ids) == 0) {
        return(NULL)
    }
    orcid_ids <- orcid_ids |>
        purrr::map_df(function(x){
            list(title = x$title,
                           orcid = x$orcid)
        }) |>
        dplyr::mutate(orcid = gsub("^.*orcid\\.org/(.*)$", "\\1", .data$orcid))

    out_folder <- file.path(tws_options()$output, "orcid")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", orcid_ids$orcid))
        remove_outfiles(files = files)
    }

    request_num <- tws_options()$author_max
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(orcid_ids))) {
        out_file <- file.path(out_folder, paste0(orcid_ids$orcid[i], ".Rds"))
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
            message("Get works from ORCID for ", orcid_ids$title[i])

            works <- rorcid::orcid_works(orcid_ids$orcid[i])

            dois <- works[[orcid_ids$orcid[i]]]$works$`external-ids.external-id`  |>
                purrr::map(function(x) {
                    tryCatch({
                        if (is.null(x) || length(x) == 0) {
                            return(NULL)
                        }
                        x2 <- x |>  dplyr::filter(.data$`external-id-type` == "doi")
                        if (nrow(x2) == 0) {
                            return(NULL)
                        }
                        if (rlang::has_name(x2, "external-id-normalized.value")) {
                            return (x2$`external-id-normalized.value`)
                        }
                        return(NULL)
                    }, error = function(e) {
                        #assign("x", x, .GlobalEnv)
                        #message(x)
                        stop(e)
                    })
                }) |>
                unlist()  |>
                unique()
            works <- tibble::tibble(title = orcid_ids$title[i],
                                    doi = dois,
                                    is_new = TRUE)
            saveRDS(works, out_file)
            Sys.sleep(1)
        }
        all_works[[i]] <- works
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()

    works_authoring(all_works, is_new)

}
