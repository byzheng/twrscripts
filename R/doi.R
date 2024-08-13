
#' Get DOIs from tiddlywiki
#'
#' @param filter filter to get dois
#' @return A data frame with two columns title and doi
#' @export
get_dois <- function(filter = NULL) {
    if (is.null(filter)) {
        f <- "[tag[bibtex-entry]!hasp[draft.of]!is[system]has[bibtex-doi]!tag[Accepted Article]!tag[Preprint]]"
    } else {
        f <- filter
    }
    tiddlers <- rtiddlywiki::get_tiddlers(f)
    if (length(tiddlers) == 0) {
        return(NULL)
    }
    # Skip entry published in recent 7 days (waiting for citation providers to update)
    dois <- tiddlers  |>
        purrr::map_df(function(x){
            d <- as.Date(x$`bibtex-date`, "%Y-%m-%d")
            if (length(d) == 0 || is.na(d) || d < Sys.Date() - 7) {
                r <- list(title = x$title,
                          doi = x$`bibtex-doi`)
            } else {
                r <- list(title = character(0),
                          doi = character(0))
            }
            r
        })

    dois <- dois  |>
        dplyr::mutate(doi = gsub("https?://.*doi\\.org/", "", .data$doi))
    dois
}

