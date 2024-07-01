
#' Title  Check duplicates of colleagues
#'
#' @return No return
#' @export
check_colleagues <- function() {

    f <- "[tag[Colleague]!has[draft.of]]"
    tiddlers <- rtiddlywiki::get_tiddlers(f)
    colleague <- tiddlers |>
        purrr::map_df(function(x){
            tibble::tibble(title = x$title,
                   email = x$email,
                   image = x$image,
                   url = x$url,
                   `google-scholar` = x$`google-scholar`,
                   orcid = x$orcid,
                   researcherid = x$researcherid,
                   linkedid = x$linkedin,
                   researchgate = x$researchgate,
                   github = x$github,
                   scopus = x$scopus,
                   scholarmate = x$scholarmate,
                   twitter = x$twitter)
        })

    fields <- names(colleague)[-1]
    i <- 1
    for (i in seq(along = fields)) {
        df <- colleague |>
            dplyr::select("title", value = fields[i]) |>
            dplyr::mutate(value = tolower(.data$value)) |>
            stats::na.omit() |>
            dplyr::filter(nchar(.data$value) > 0)
        duplicate <- df |>
            dplyr::count(.data$value) |>
            dplyr::filter(.data$n > 1)
        if (nrow(duplicate) > 0) {
            duplicate |>
                dplyr::left_join(df, by = "value") |>
                as.data.frame() |>
                print()
            warning(fields[i], " has duplicates...")
        }
    }
}
