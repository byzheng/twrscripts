
#' Get DOIs from tiddlywiki
#'
#' @return A data frame with two columns title and doi
#' @export
get_dois <- function() {
    f <- "[tag[bibtex-entry]!hasp[draft.of]!is[system]has[bibtex-doi]]"
    tiddlers <- rtiddlywiki::get_tiddlers(f)
    if (length(tiddlers) == 0) {
        return(NULL)
    }
    dois <- tiddlers  |>
        purrr::map_df(function(x){
            list(title = x$title,
                   doi = x$`bibtex-doi`)
        })

    dois <- dois  |>
        dplyr::mutate(doi = gsub("https?://.*doi\\.org/", "", .data$doi))
    dois
}

