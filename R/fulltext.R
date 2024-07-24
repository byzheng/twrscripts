fulltext_scopus <- function() {
    # Get all tiddlers

    out_folder <- file.path(tws_options()$output, "fulltext")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    all_dois <- get_dois(filter = "[tag[bibtex-entry]!hasp[draft.of]!is[system]has[bibtex-doi]!tag[Accepted Article]] :filter[get[bibtex-journaltitle]pubname[]tag[Elsevier]]") |>
        dplyr::mutate(file = file.path(out_folder, paste0(.data$title, ".Rds"))) |>
        dplyr::filter(!file.exists(file))

    i <- 1
    for (i in seq(along = all_dois[[1]])) {
        message("Get fulltext from scopus for doi ", all_dois$doi[i], " with ", all_dois$title[i])
        tryCatch({

            works <- rscopus::article_retrieval(id = all_dois$doi[i],
                                                identifier = "doi", view = "FULL")
            saveRDS(works,  all_dois$file[i])

            Sys.sleep(1)
        }, error = function(e) {
            warning(e)
        })
    }

    return(invisible())
}


