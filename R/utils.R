
#' Remove old intermediate files by dates
#'
#' @description
#' Intermediate files are expired by number of days (defined by option `file_expired`, 90 days in default).
#' As the limitation of web API, only a few number of files are removed (define by option `file_remove_max`, 3 in default).
#'
#'
#' @param files a list of files
#' @param expired_days days to expire
#' @param file_remove_max maximum files to remove
#'
#' @return no return
remove_outfiles <- function(files, expired_days = NULL, file_remove_max = NULL) {
    if (is.null(expired_days)) {
        expired_days <- tws_options()$file_expired
    } else {
        stopifnot(is.numeric(expired_days))
        stopifnot(length(expired_days) == 1)
    }
    if (is.null(file_remove_max)) {
        file_remove_max <- tws_options()$file_remove_max
    } else {
        stopifnot(is.numeric(file_remove_max))
        stopifnot(length(file_remove_max) == 1)
    }
    max_files <- file_remove_max
    files <- files[file.exists(unique(files))]
    files_info <- file.info(files)
    files_info <- files_info |>
        tibble::as_tibble() |>
        dplyr::mutate(file = files) |>
        dplyr::arrange(.data$mtime) |>
        dplyr::filter((as.numeric(Sys.time()) - as.numeric(.data$mtime)) > expired_days * 24 * 3600)
    if (nrow(files_info) == 0) {
        return(invisible())
    }

    pos <- seq_len(min(c(max_files, nrow(files_info))))
    files_remove <- files_info  |>
        dplyr::slice(pos)

    message("Remove files: ", paste(basename(files_remove$file), collapse = ", "))
    file.remove(files_remove$file)
    return(invisible())
}



url_id <- function(url, name = NULL) {
    # url <- tolower(url)
    # if (!is.null(name)) {
    #     name <- tolower(name)
    # }
    res <- c()
    for (i in seq(along = url)) {
        url_i <- rtiddlywiki::split_field(url[i])
        all_v <- c()
        for (j in seq(along = url_i)) {
            parsed <- httr2::url_parse(url_i[j])
            if (is.null(name)) {
                v <- basename(parsed$path)
            } else {
                v <- parsed$query[[name]]
            }
            all_v <- c(all_v, v)
        }
        res[i] <- paste(all_v, collapse = " ")
    }
    res
}
