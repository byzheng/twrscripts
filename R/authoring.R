
#' Title Add authors for works
#'
#' @description
#' Tiddlers with tag bibtex-entry will be updated for new authors.
#' @param works a data frame for works
#' @param is_new whether only to process new items
#'
#' @return No return
works_authoring <- function(works, is_new = FALSE) {

    # Check works
    stopifnot(is.data.frame(works))
    stopifnot(rlang::has_name(works, "title"))
    stopifnot(rlang::has_name(works, "doi"))

    if (is_new) {
        works <- works  |>
            dplyr::filter(is_new)
    }
    if (nrow(works) == 0) {
        message("No new authors")
        return(invisible())
    }

    dois <- get_dois() |>
        dplyr::rename(key = .data$title)

    authors_key <- works  |>
        dplyr::rename(author = .data$title) |>
        dplyr::left_join(dois, by = "doi")  |>
        dplyr::filter(!is.na(.data$key))


    # Get authors for entry
    bibtex_tiddlers <- authors_key |>
        dplyr::count(.data$key) |>
        dplyr::arrange(dplyr::desc(.data$n))
    i <- 1
    for (i in seq(along = bibtex_tiddlers[[1]])) {
        #if (bibtex_tiddlers$key[i] == "steinfort_vernalisation_2017") stop()
        authors <- bibtex_tiddlers |>
            dplyr::slice(i) |>
            dplyr::left_join(authors_key, by = "key")  |>
            dplyr::pull(.data$author) |>
            unique()

        tiddler <- rtiddlywiki::get_tiddler(bibtex_tiddlers$key[i])
        # Check whether it is required to update
        missing <- !(authors %in% tiddler$tags)
        if (sum(missing) > 0) {
            message("Updating: ", bibtex_tiddlers$key[i], " for new authors \"", authors[missing], "\"")
            new_tags <- unique(c(authors, tiddler$tags))
            rtiddlywiki::put_tiddler(bibtex_tiddlers$key[i], tags = new_tags)
        }
    }
    return(invisible())

}
