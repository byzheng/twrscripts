
#' Get reference list for publications from crossref
#'
#' @details
#' The reference list is obtained from crossref. The field `reference` in Tiddlywiki is updated
#' to list all reference in the Tiddlywiki. Two fields `reference-count` and `cited-count` is also updated
#' for number of reference and citation for a publication, respectively.
#'
#'
#' @return no return
#' @export
reference <- function() {
    # Get all tiddlers

    all_dois <- get_dois()

    out_folder <- file.path(tws_options()$output, "reference")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    out_file <- file.path(out_folder, "crossref_reference.Rds")
    if (file.exists(out_file)) {
        all_refs <- readRDS(out_file)
    } else {
        all_refs <- data.frame(title = character(0), doi = character(0))
    }

    # only process missing dois for crossref
    dois <- all_dois[!(all_dois$title %in% unique(all_refs$title)),]

    if (nrow(dois) == 0) {
        return(NULL)
    }
    i <- 1
    new_refs <- list()
    for (i in seq(along = dois[[1]])) {
        message("Get reference from crossref for doi ", dois$doi[i], " with ", dois$title[i])
        works <- rcrossref::cr_works(dois = dois$doi[i])
        if (!rlang::has_name(works$data, "reference") ||
            !rlang::has_name(works$data$reference[[1]], "DOI")) {
            refs_i <- tibble::tibble(title = dois$title[i],
                             doi = "")
        } else {
            refs_i <- tibble::tibble(title = dois$title[i],
                             doi = works$data$reference[[1]]$DOI)
        }
        new_refs[[i]] <- refs_i
        Sys.sleep(1)
    }
    new_refs <- new_refs |>
        dplyr::bind_rows() |>
        dplyr::filter(!is.na(.data$doi))
    all_refs <- all_refs |>
        dplyr::bind_rows(new_refs) |>
        dplyr::distinct()


    # update new reference list
    cited_title <- c()
    for (i in seq(along = dois[[1]])) {
        new_refs_i <- new_refs[new_refs$title == dois$title[i],]
        if (nrow(new_refs_i) == 0) {
            break
        }
        refs_tiddler <- all_dois[all_dois$doi %in% new_refs_i$doi,]
        if (nrow(refs_tiddler) > 0) {
            rtiddlywiki::put_tiddler(dois$title[i],
                        fields = list(reference = refs_tiddler$title,
                                      `reference-count` = length(refs_tiddler$title)))
            cited_title <- c(cited_title, refs_tiddler$title)
        }
        # Update literature who cite this paper
        refs_title <- all_refs |>
            dplyr::filter(.data$doi %in% dois$doi[i] & .data$title != dois$title[i]) |>
            dplyr::distinct(.data$title) |>
            dplyr::pull(.data$title)
        j <- 1
        for (j in seq(along = refs_title)) {
            refs_tiddler <- all_refs |>
                dplyr::filter(.data$title == refs_title[j])
            refs_tiddler <- all_dois[all_dois$doi %in% refs_tiddler$doi,]
            rtiddlywiki::put_tiddler(refs_title[j],
                        fields = list(reference = refs_tiddler$title,
                                      `reference-count` = length(refs_tiddler$title)))

        }
        if (length(refs_title) > 0) {
            cited_title <- c(cited_title, dois$title[i])
        }
    }


    # Update cited number
    # cited_title <- all_dois[all_dois$doi %in% new_refs$doi,] |>
    #     distinct(title) |>
    #     pull(title)

    for (i in seq(cited_title)) {
        f <- sprintf("[tag[bibtex-entry]has[reference]] :filter[get[reference]enlist-input[]match[%s]]",
                     cited_title[i])
        cited <- rtiddlywiki::get_tiddlers(f)
        rtiddlywiki::put_tiddler(cited_title[i],
                    fields = list(`cited-count` = length(cited)))
    }

    saveRDS(all_refs, out_file)
    return(invisible())
}
