
crossref <- function(remove_old = TRUE, all = FALSE) {
    # Get all tiddlers

    all_dois <- get_dois()

    out_folder <- file.path(tws_options()$output, "reference")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    out_file <- file.path(out_folder, "crossref_reference.Rds")
    if (file.exists(out_file)) {
        all_refs <- readRDS(out_file)
        if (!tibble::has_name(all_refs, "update_date")) {
            all_refs$update_date <- Sys.Date()
        }
        # all_refs <- all_refs |>
        #     dplyr::filter(title != "nelson_quantitative_2014")
    } else {
        all_refs <- data.frame(title = character(0), doi = character(0),
                               update_date = as.Date(numeric(0)))
    }

    if (remove_old) {
        # remove 30 old records if more than 30 days
        old_refs <- all_refs |>
            dplyr::distinct(.data$title, .data$update_date) |>
            dplyr::filter((as.numeric(Sys.Date()) - as.numeric(.data$update_date)) > 30) |>
            dplyr::slice(seq_len(30)) |>
            dplyr::select("title")
        if (nrow(old_refs) > 0) {
            all_refs <- all_refs |>
                dplyr::anti_join(old_refs, by = "title")
        }
    }


    # only process missing dois for crossref
    dois <- all_dois[!(all_dois$title %in% unique(all_refs$title)),]
    new_refs <- list()
    if (nrow(dois) > 0) {
        i <- 1
        for (i in seq(along = dois[[1]])) {
            message("Get reference from crossref for doi ", dois$doi[i], " with ", dois$title[i])
            works <- rcrossref::cr_works(dois = dois$doi[i])
            if (!rlang::has_name(works$data, "reference") ||
                !rlang::has_name(works$data$reference[[1]], "DOI")) {
                refs_i <- tibble::tibble(title = dois$title[i],
                                 doi = "",
                                 update_date = Sys.Date())
            } else {
                refs_i <- tibble::tibble(title = dois$title[i],
                                 doi = works$data$reference[[1]]$DOI,
                                 update_date = Sys.Date())
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
        saveRDS(all_refs, out_file)
    }

    if (!all) {
        if (length(new_refs) == 0) {
            return(invisible())
        }
        updated_refs <- all_refs |>
            dplyr::filter(.data$doi %in% dois$doi) |>
            dplyr::bind_rows(new_refs) |>
            dplyr::pull(.data$title) |>
            unique()
        all_refs <- all_refs |>
            dplyr::filter(.data$title %in% updated_refs)
    }
    refs <- all_refs |>
        dplyr::left_join(all_dois |>
                              dplyr::rename(reference = "title"), by = "doi") |>
        dplyr::filter(!is.na(.data$reference)) |>
        dplyr::select("title", "reference") |>
        dplyr::distinct() |>
        dplyr::filter(.data$title %in% all_dois$title)

    # Existing entry to cite this one

    return(refs)
}


opencitations <- function(remove_old = TRUE, all = FALSE) {
    all_dois <- get_dois()

    out_folder <- file.path(tws_options()$output, "opencitations")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }

    out_file <- file.path(out_folder, "opencitations.Rds")
    if (file.exists(out_file)) {
        all_refs <- readRDS(out_file) |>
            tibble::tibble()
        # all_refs <- all_refs |>
        #     dplyr::filter(title != "bottcher_phenological_2016")
    } else {
        all_refs <- data.frame(title = character(0),
                                    doi = character(0),
                                    update_date = as.Date(numeric(0)))
    }

    if (remove_old) {
        # remove 10 old records if more than 30 days
        old_refs <- all_refs |>
            dplyr::distinct(.data$title, .data$update_date) |>
            dplyr::filter((as.numeric(Sys.Date()) - as.numeric(.data$update_date)) > 30) |>
            dplyr::slice(seq_len(30)) |>
            dplyr::select("title")
        if (nrow(old_refs) > 0) {
            all_refs <- all_refs |>
                dplyr::anti_join(old_refs, by = "title")
        }
    }
    # only process missing dois for crossref
    dois <- all_dois[!(all_dois$title %in% unique(all_refs$title)),]

    new_refs <- list()
    if (nrow(dois) > 0) {

        # Get data from opencitations for missing dois
        i <- 1

        for (i in seq(along = dois[[1]])) {
            message("Get reference list  from opencitations for doi ", dois$doi[i], " with ", dois$title[i])
            x <- try({
                url <- sprintf("https://opencitations.net/index/coci/api/v1/references/%s", dois$doi[i])
                references <- httr::GET(url = url)
                references <- httr::content(references)
                if (length(references) == 0) {
                    references <- data.frame(title = dois$title[i],
                                             doi = NA,
                                            update_date = Sys.Date())
                } else {
                    references <- references |> purrr::map_df(function(x) {
                        list(doi = x$cited)
                    }) |>
                        dplyr::mutate(title = dois$title[i],
                                      update_date = Sys.Date())
                }
                #Sys.sleep(1)
            })
            if (inherits(x, "try-error")) {
                references <- data.frame(title = dois$title[i],
                                         doi = NA,
                                        update_date = Sys.Date())
            }
            new_refs[[i]] <- references
        }
        new_refs <- new_refs |>
            dplyr::bind_rows() |>
            dplyr::mutate(update_date = Sys.Date())
        all_refs <- all_refs |>
            dplyr::bind_rows(new_refs) |>
            dplyr::distinct()


        saveRDS(all_refs, out_file)
    }

    if (!all) {
        if (length(new_refs) == 0) {
            return(invisible())
        }
        updated_refs <- all_refs |>
            dplyr::filter(.data$doi %in% dois$doi) |>
            dplyr::bind_rows(new_refs) |>
            dplyr::pull(.data$title) |>
            unique()
        all_refs <- all_refs |>
            dplyr::filter(.data$title %in% updated_refs)
    }
    refs <- all_refs |>
        dplyr::left_join(all_dois |>
                             dplyr::rename(reference = "title"), by = "doi") |>
        dplyr::filter(!is.na(.data$reference)) |>
        dplyr::select("title", "reference") |>
        dplyr::distinct() |>
        dplyr::filter(.data$title %in% all_dois$title)

    # Existing entry to cite this one

    return(refs)
}


#' Get reference list for publications from crossref and opencitations
#'
#' @details
#' The reference list is obtained from crossref and opencitations. The field `reference` in Tiddlywiki is updated
#' to list all reference in the Tiddlywiki. Two fields `reference-count` and `cited-count` is also updated
#' for number of reference and citation for a publication, respectively.
#'
#' @param remove_old whether to remove old files
#' @param all whether to get all reference
#' @return no return
#' @export
reference <- function(remove_old = TRUE, all = FALSE) {
    ref1 <- crossref(remove_old = remove_old, all = all)
    ref2 <- opencitations(remove_old = remove_old, all = all)
    if (is.null(ref1) && is.null(ref2)) {
        return(invisible())
    }
    all_refs <- dplyr::bind_rows(ref1, ref2) |>
        dplyr::distinct()

    tiddlers_update <- all_refs |>
        dplyr::distinct(.data$title)

    # update new reference list
    i <- 1
    for (i in seq(along = tiddlers_update[[1]])) {
        refs_tiddler <- all_refs |>
            dplyr::filter(.data$title == tiddlers_update$title[i])
        if (nrow(refs_tiddler) == 0) {
            break
        }

        old_tiddler <- rtiddlywiki::get_tiddler(tiddlers_update$title[i])
        all_refs_i <- rtiddlywiki::split_field(old_tiddler$fields$reference)
        all_refs_i <- unique(c(all_refs_i, refs_tiddler$reference))
        rtiddlywiki::put_tiddler(tiddlers_update$title[i],
                    fields = list(reference = all_refs_i,
                                  `reference-count` = length(all_refs_i)))
    }

    cited_title <- unique(all_refs$reference)

    i <- 1
    for (i in seq(cited_title)) {
        f <- sprintf("[tag[bibtex-entry]has[reference]] :filter[get[reference]enlist-input[]match[%s]]",
                     cited_title[i])
        cited <- rtiddlywiki::get_tiddlers(f)
        rtiddlywiki::put_tiddler(cited_title[i],
                                 fields = list(`cited-count` = length(cited)))
    }

    return(invisible())
}

