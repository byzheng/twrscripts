
#' Get works from homepage
#'
#'
#' @details
#' It assumes all dois in the homepage are colleague's publication. The field `url` is to
#' define the homepage.
#'
#' The retrieved works are stored into subfolder `homepage` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from homepage
#' @export
works_homepage <- function(is_new = FALSE) {

    f <- "[tag[Colleague]has[url]!has[draft.of]]"

    homepages <- rtiddlywiki::get_tiddlers(f)
    if (length(homepages) == 0) {
        return(NULL)
    }
    homepages <- homepages |>
        purrr::map_df(function(x){
            list(title = x$title,
                 url = x$url)
        }) |>
        dplyr::filter(!is.na(.data$url), nchar(.data$url) > 0)

    out_folder <- file.path(tws_options()$output, "homepage")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", homepages$title))
        remove_outfiles(files = files)
    }

    regex <- '10\\.\\d{4,9}/[-._;()/:a-zA-Z0-9]+'
    httr::set_config(httr::config(ssl_verifypeer = 0L))

    request_num <- tws_options()$author_max
    request_num <- nrow(homepages) + 1
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(homepages))) {
        out_file <- file.path(out_folder, paste0(homepages$title[i], ".Rds"))
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
            message("Get works from homepage for ", homepages$title[i])

            tryCatch({
                message(homepages$title[i])
                response <- httr::GET(homepages$url[i])
                cont <- httr::content(response)

                cont_str <- as.character(cont)
                dois <- stringi::stri_extract_all(cont_str, regex = regex)  |>
                    unique()
                works <- tibble::tibble(title = homepages$title[i], doi = dois[[1]])
                works$is_new <- TRUE
                saveRDS(works, out_file)
            }, error = function(e){
                works <- tibble::tibble(title = homepages$title[i], doi = NA)
                works$is_new <- FALSE
                saveRDS(works, out_file)
                message(e)
            })
            Sys.sleep(1)

        }
        all_works[[i]] <- works
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()

    all_works <- all_works |>
        dplyr::filter(!is.na(.data$doi))
    works_authoring(all_works, is_new)

}
