
#' Remove old intermediate files by dates
#'
#' @description
#' Intermediate files are expired by number of days (defined by option `file_expired`, 90 days in default).
#' As the limitation of web API, only a few number of files are removed (define by option `file_remove_max`, 3 in default).
#'
#'
#' @param files a list of files
#'
#' @return no return
remove_outfiles <- function(files) {
    expired_days <- tws_options()$file_expired
    max_files <- tws_options()$file_remove_max
    files <- files[file.exists(unique(files))]
    files_info <- file.info(files)
    files_info <- files_info |>
        tibble::as_tibble() |>
        dplyr::mutate(file = files) |>
        dplyr::arrange(.data$ctime) |>
        dplyr::filter((as.numeric(Sys.time()) - as.numeric(.data$ctime)) > expired_days * 24 * 3600)
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
