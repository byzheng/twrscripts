
crossref <- function(remove_old = TRUE) {
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

    if (nrow(dois) == 0) {
        return(invisible())
    }
    i <- 1
    new_refs <- list()
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


    updated_refs <- all_refs |>
        dplyr::filter(.data$doi %in% dois$doi) |>
        dplyr::bind_rows(new_refs) |>
        dplyr::pull(.data$title) |>
        unique()


    refs <- all_refs |>
        dplyr::filter(.data$title %in% updated_refs) |>
        dplyr::left_join(all_dois |>
                              dplyr::rename(reference = "title"), by = "doi") |>
        dplyr::filter(!is.na(.data$reference)) |>
        dplyr::select("title", "reference") |>
        dplyr::distinct() |>
        dplyr::filter(.data$title %in% all_dois$title)

    # Existing entry to cite this one

    return(refs)
}


opencitations <- function(remove_old = TRUE) {
    all_dois <- get_dois()

    out_folder <- file.path(tws_options()$output, "opencitations")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }

    out_file <- file.path(out_folder, "opencitations.Rds")
    if (file.exists(out_file)) {
        all_citations <- readRDS(out_file) |>
            tibble::tibble()
        # all_citations <- all_citations |>
        #     dplyr::filter(title != "nelson_quantitative_2014")
    } else {
        all_citations <- data.frame(creation = character(0), citing = character(0),
                                    cited = character(0),
                                    title = character(0),
                                    update_date = as.Date(numeric(0)))
    }

    if (remove_old) {
        # remove 10 old records if more than 30 days
        old_citations <- all_citations |>
            dplyr::distinct(.data$title, .data$update_date) |>
            dplyr::filter((as.numeric(Sys.Date()) - as.numeric(.data$update_date)) > 30) |>
            dplyr::slice(seq_len(30)) |>
            dplyr::select("title")
        if (nrow(old_citations) > 0) {
            all_citations <- all_citations |>
                dplyr::anti_join(old_citations, by = "title")
        }
    }
    # only process missing dois for crossref
    dois <- all_dois[!(all_dois$title %in% unique(all_citations$title)),]

    if (nrow(dois) == 0) {
        return(invisible())
    }

    # Get data from opencitations for missing dois
    i <- 1
    new_citations <- list()
    for (i in seq(along = dois[[1]])) {
        message("Get citation list  from opencitations for doi ", dois$doi[i], " with ", dois$title[i])
        x <- try({
            url <- sprintf("https://opencitations.net/index/coci/api/v1/citations/%s", dois$doi[i])
            citations <- httr::GET(url = url)
            citations <- httr::content(citations)
            if (length(citations) == 0) {
                citations <- data.frame(creation = NA, citing = NA,
                                        cited = dois$doi[i],
                                        title = dois$title[i],
                                        update_date = Sys.Date())
            } else {
                citations <- citations |> purrr::map_df(function(x) {
                    list(creation = x$creation, citing = x$citing, oci = x$oci)
                }) |>
                    dplyr::mutate(cited = dois$doi[i],
                                  title = dois$title[i],
                                  update_date = Sys.Date())
            }
            #Sys.sleep(1)
        })
        if (inherits(x, "try-error")) {
            citations <- data.frame(creation = NA, citing = NA,
                                    cited = dois$doi[i],
                                    title = dois$title[i],
                                    update_date = Sys.Date())
        }
        new_citations[[i]] <- citations
    }
    new_citations <- new_citations |>
        dplyr::bind_rows() |>
        dplyr::mutate(update_date = Sys.Date())
    all_citations <- all_citations |>
        dplyr::bind_rows(new_citations) |>
        dplyr::distinct()


    saveRDS(all_citations, out_file)

    updated_refs <- new_citations |>
        dplyr::filter(.data$citing %in% all_dois$doi)

    all_refs <- all_citations |>
        dplyr::filter(.data$citing %in% updated_refs$citing) |>
        dplyr::filter(!is.na(.data$citing)) |>
        dplyr::select("cited_doi" = "cited", "doi" = "citing") |>
        dplyr::distinct() |>
        dplyr::right_join(all_dois, by = "doi") |>
        dplyr::select("title", "doi" = "cited_doi") |>
        dplyr::arrange(.data$title) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(.data$doi)) |>
        dplyr::right_join(all_dois |> dplyr::rename(reference = "title"), by = "doi") |>
        dplyr::select("title", "reference") |>
        dplyr::distinct() |>
        dplyr::filter(.data$title %in% all_dois$title)

    # Merge data from crossref
    crossref_folder <- file.path(tws_options()$output, "reference")
    crossref_file <- file.path(crossref_folder, "crossref_reference.Rds")
    if (!file.exists(crossref_file)) {
        stop("Cannot find file ", crossref_file)
    }
    crossref <- readRDS(crossref_file)

    all_refs <- crossref |>
        dplyr::filter(.data$title %in% all_refs$title) |>
        dplyr::left_join(all_dois |>
                             dplyr::rename(reference = "title"), by = "doi") |>
        dplyr::filter(!is.na(.data$reference)) |>
        dplyr::select("title", "reference") |>
        dplyr::distinct() |>
        dplyr::filter(.data$title %in% all_dois$title) |>
        dplyr::full_join(all_refs, by = c("title", "reference")) |>
        dplyr::distinct()
    return(all_refs)
}


#' Get reference list for publications from crossref and opencitations
#'
#' @details
#' The reference list is obtained from crossref and opencitations. The field `reference` in Tiddlywiki is updated
#' to list all reference in the Tiddlywiki. Two fields `reference-count` and `cited-count` is also updated
#' for number of reference and citation for a publication, respectively.
#'
#' @param remove_old whether to remove old files
#' @return no return
#' @export
reference <- function(remove_old = TRUE) {
    ref1 <- crossref(remove_old = remove_old)
    ref2 <- opencitations(remove_old = remove_old)
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

        rtiddlywiki::put_tiddler(tiddlers_update$title[i],
                    fields = list(reference = refs_tiddler$reference,
                                  `reference-count` = length(refs_tiddler$reference)))
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

