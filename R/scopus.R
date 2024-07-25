

#' Find author ids from SCOPUS
#'
#' @param threshold Threshold for similarity
#' @param update whether update tiddlywiki
#'
#' @return A data.frame with new author id
#' @export
author_scopus <- function(threshold = 0.9, update = FALSE) {
    stopifnot(length(threshold) == 1)
    stopifnot(is.numeric(threshold))
    stopifnot(threshold > 0.7 && threshold <= 1)
    stopifnot(length(update) == 1)
    stopifnot(is.logical(update))

    get_author_scopus(remove_old = TRUE)
    out_folder <- file.path(tws_options()$output, "reference")
    out_file_author <- file.path(out_folder, "scopus_authors.Rds")
    if (!file.exists(out_file_author)) {
        stop("Cannot find scopus author file. Call ref_scopus first")
    }
    scopus_info <- readRDS(out_file_author)
    scopus_info <- scopus_info |>
        dplyr::filter(!is.na(.data$authorid)) |>
        dplyr::mutate(given = gsub("^(.*)( [a-zA-Z]{1}\\.$)", "\\1", .data$given))
    # colleagues without empty scopus field
    colleagues <- rtiddlywiki::get_tiddlers("[tag[Colleague]!has[scopus]!has[draft.of]] :filter[all[tiddlers]tag[bibtex-entry]tag<currentTiddler>]")
    message("Number of colleagues without SCOPUS author id but with publications: ")
    message(length(colleagues))

    find_aid <- list()
    i <- 1
    for (i in seq(along = colleagues)) {
        # get publications
        pubs <- rtiddlywiki::get_tiddlers(sprintf("[tag[%s]tag[bibtex-entry]!has[draft.of]]", colleagues[[i]]$title))
        if (length(pubs) == 0) {
            next
        }
        pubs_aid <- pubs |>
            purrr::map_df(function(x) list(title = x$title)) |>
            dplyr::left_join(scopus_info, by = "title") |>
            dplyr::filter(!is.na(.data$authorid))
        if (nrow(pubs_aid) == 0) {
            next
        }
        is_chinese <- grep("[\\p{Han}]", colleagues[[i]]$title, value = TRUE, perl = TRUE)
        if (length(is_chinese) == 0) {
            pubs_aid <- pubs_aid |>
                dplyr::mutate(fullname = paste(.data$given, .data$surname),
                              colleague = colleagues[[i]]$title) |>
                dplyr::mutate(sim = stringdist::stringsim(tolower(.data$fullname),
                                                          tolower(.data$colleague))) |>
                dplyr::arrange(dplyr::desc(.data$sim))
        } else {
            pubs_aid <- pubs_aid |>
                dplyr::mutate(fullname = paste(.data$surname, .data$given, sep = ""),
                              colleague = colleagues[[i]]$title)

            tiddler <- rtiddlywiki::get_tiddler(colleagues[[i]]$title)
            if (is.null(tiddler$fields$aka)) {
                next
            }
            akas <- rtiddlywiki::split_field(tiddler$fields$aka)
            pubs_aid$sim <- rep(0, nrow(pubs_aid))
            for (j in seq(along = akas)) {
                sim_j <- stringdist::stringsim(tolower(pubs_aid$fullname),
                                               tolower(akas[j]))
                pubs_aid$sim <- pmax(pubs_aid$sim, sim_j)
            }
            pubs_aid <- pubs_aid|>
                dplyr::arrange(dplyr::desc(.data$sim))
        }
        pubs_aid <- pubs_aid |>
            dplyr::filter(.data$sim > threshold)
        if (nrow(pubs_aid)) {
            authorid <- unique(pubs_aid$authorid)
            if (length(authorid) > 1) {
                pubs_aid |>
                    dplyr::select(.data$title, .data$authorid, .data$fullname, .data$colleague) |>
                    print()
                stop("Mulitple author ids are found.")
            }
            #message("Find ", nrow(pubs_aid), " records for author id for ", colleagues[[i]]$title)
            find_aid[[length(find_aid) + 1]] <- pubs_aid |>
                dplyr::count(.data$colleague, .data$given, .data$surname, .data$authorid, .data$sim)

        }
    }
    if (length(find_aid) == 0) {
        message("Do not find any new author ids")
        return(NULL)
    }
    find_aid <- find_aid |>
        dplyr::bind_rows(find_aid) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$sim) |>
        dplyr::group_by(.data$authorid) |>
        dplyr::mutate(n = sum(.data$n)) |>
        dplyr::filter(.data$sim == max(.data$sim))

    if (update) {
        for (i in seq_len(nrow(find_aid))) {
            authorid <- sprintf("https://www.scopus.com/authid/detail.uri?authorId=%s",
                                find_aid$authorid[i])
            message("Update SCOPUS author id for ", find_aid$colleague[i], " with ",
                    find_aid$authorid[i])
            message(authorid)
            rtiddlywiki::put_tiddler(find_aid$colleague[i],
                         fields = list(scopus = authorid))
        }
    }
    return(find_aid)

}

#' Get works from Scopus using Scopus API.
#'
#' @details
#' R package `rscopus` is used to retrieve works from Scopus according to `scopus`
#' field in Tiddlywiki.
#'
#' The retrieved works are stored into subfolder `scopus` in the output folder which
#' is defined by option `output`.
#'
#' The works will be re-downloaded after 90 days (modifying through option file_expired)
#' if `is_new` equals to `FALSE`
#'
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return A data frame for all works obtained from Scopus
#' @export
works_scopus <- function(is_new = FALSE) {
    f <- "[tag[Colleague]has[scopus]!has[draft.of]]"

    scopus_ids <- rtiddlywiki::get_tiddlers(f)
    if (length(scopus_ids) == 0) {
        return(NULL)
    }

    scopus_ids <- scopus_ids |>
        purrr::map_df(function(x){
            tibble::tibble(title = x$title,
                   scopus = x$scopus)
        }) |>
        dplyr::mutate(scopus = url_id(.data$scopus, "authorId"))

    out_folder <- file.path(tws_options()$output, "scopus")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (!is_new) {
        # Check out files
        files <- file.path(out_folder, sprintf("%s.Rds", scopus_ids$scopus))
        remove_outfiles(files = files)
    }

    request_num <- tws_options()$author_max
    request_num_now <- 0
    all_works <- list()
    i <- 1
    for (i in seq_len(nrow(scopus_ids))) {
        ids <- strsplit(scopus_ids$scopus[i], " ")[[1]]
        j <- 1
        for (j in seq(along = ids)) {
            out_file <- file.path(out_folder, paste0(ids[j], ".Rds"))
            if (file.exists(out_file)) {
                works <- readRDS(out_file) |>
                    dplyr::mutate(is_new = FALSE)
            } else {
                request_num_now <- request_num_now + 1
                if (request_num_now > request_num) {
                    message("Request number is more than the limit ", request_num)
                    message("Call this function again for more request")
                    break
                }
                message("Get works from SCOPUS for ", scopus_ids$title[i],
                        " with id ", ids[j])
                works <- rscopus::author_df(au_id = ids[j], verbose = FALSE) |>
                    dplyr::mutate(is_new = TRUE)
                saveRDS(works, out_file)
                Sys.sleep(1)
            }
            works <- works |>
                dplyr::select(doi = "prism:doi", is_new) |>
                dplyr::mutate(title = scopus_ids$title[i])
            all_works[[i]] <- works
        }
    }
    all_works <- dplyr::bind_rows(all_works) |> tibble::as_tibble()

    works_authoring(all_works, is_new)

    return(invisible())
}




get_author_scopus <- function(remove_old = TRUE) {
    # Get all tiddlers

    all_dois <- get_dois()

    out_folder <- file.path(tws_options()$output, "reference")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    # out_file_ref <- file.path(out_folder, "scopus_reference.Rds")
    # if (file.exists(out_file_ref)) {
    #
    #     all_refs <- readRDS(out_file_ref) |> tibble::tibble()
    #     if (!tibble::has_name(all_refs, "update_date")) {
    #         all_refs$update_date <- Sys.Date()
    #     }
    #     # all_refs <- all_refs |>
    #     #     dplyr::filter(title != "nelson_quantitative_2014")
    # } else {
    #     all_refs <- data.frame(title = character(0), doi = character(0),
    #                            update_date = as.Date(numeric(0)))
    # }

    out_file_author <- file.path(out_folder, "scopus_authors.Rds")
    if (file.exists(out_file_author)) {

        all_authors <- readRDS(out_file_author) |> tibble::tibble()
        if (!tibble::has_name(all_authors, "update_date")) {
            all_authors$update_date <- Sys.Date()
        }
        # all_refs <- all_refs |>
        #     dplyr::filter(title != "nelson_quantitative_2014")
    } else {
        all_authors <- data.frame(given = character(0), surname = character(0),
                                  authorid = character(0),
                                  title = character(0),
                                  update_date = as.Date(numeric(0)))
    }

    if (remove_old) {
        # remove 30 old records if more than 30 days
        old_refs <- all_authors |>
            dplyr::distinct(.data$title, .data$update_date) |>
            dplyr::filter((as.numeric(Sys.Date()) - as.numeric(.data$update_date)) > 30) |>
            dplyr::slice(seq_len(30)) |>
            dplyr::select("title")
        if (nrow(old_refs) > 0) {
            # all_refs <- all_refs |>
            #     dplyr::anti_join(old_refs, by = "title")
            all_authors <- all_authors |>
                dplyr::anti_join(old_refs, by = "title")

        }
    }


    # only process missing dois for crossref
    dois <- all_dois[!(all_dois$title %in% unique(all_authors$title)),]

    max_request <- 4000
    request_num <- 0
    #new_refs <- list()
    new_authors <- list()
    if (nrow(dois) > 0) {
        i <- 1
        for (i in seq(along = dois[[1]])) {
            tryCatch({

                message("Get reference from scopus for doi ", dois$doi[i], " with ", dois$title[i])
                works <- rscopus::embase_retrieval(id = dois$doi[i], identifier = "doi", verbose = FALSE)

                if (length(works$content) == 0 || httr::status_code(works$get_statement) == 404) {
                    eid <- ""
                } else {
                    eid <- works$content$`abstracts-retrieval-response`$coredata$eid
                }
                rtiddlywiki::put_tiddler(dois$title[i], fields = list(`scopus-eid` = eid))

                if (length(works$content) == 0 || httr::status_code(works$get_statement) == 404) {
                    authors_i <- data.frame(given = NA, surname = NA,
                                            authorid = NA,
                                            title = dois$title[i],
                                            update_date = Sys.Date())
                    # refs_i <- data.frame(title =  dois$title[i], doi = NA,
                    #                        update_date = Sys.Date())
                } else {
                    authors_i <- works$content$`abstracts-retrieval-response`$authors$author |>
                        purrr::map_df(function(x) list(given = x$`ce:given-name`, surname = x$`ce:surname`,
                                                authorid = x$`@auid`)) |>
                        dplyr::mutate(title = dois$title[i],
                                      update_date = Sys.Date())
                    if (nrow(authors_i) == 0) {
                        authors_i <- data.frame(given = NA, surname = NA,
                                                authorid = NA,
                                                title = dois$title[i],
                                                update_date = Sys.Date())
                    }
                    # x <- works$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference[[1]]
                    # refs_i <- works$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference |>
                    #     purrr::map_df(function(x) {
                    #         tryCatch({
                    #             doi <- NA
                    #             itemids <- x$`ref-info`$`refd-itemidlist`$itemid
                    #             if (!is.null(itemids$`$`) ) {
                    #                 if (tolower(itemids$`@idtype` == "doi")) {
                    #                     doi <- itemids$`$`
                    #                 }
                    #                 return(list(doi = doi))
                    #             }
                    #
                    #             for (j in seq(along = itemids)) {
                    #                 if (tolower(itemids[[j]]$`@idtype`) == "doi") {
                    #                     doi <- itemids[[j]]$`$`
                    #                     break
                    #                 }
                    #             }
                    #             list(doi = doi)
                    #         }, error = function(e) {
                    #             #assign("x", x, .GlobalEnv)
                    #             #stop(e)
                    #         })
                    #     }) |>
                    #     dplyr::mutate(title = dois$title[i],
                    #            update_date = Sys.Date())
                }
                #new_refs[[length(new_refs) + 1]] <- refs_i
                new_authors[[length(new_authors) + 1]] <- authors_i
                Sys.sleep(1)
                request_num <- request_num + 1
                print(request_num)
                if (request_num > max_request) {
                    break
                }
            }, error = function(e) {
                stop(e)
            })
            # if (!rlang::has_name(works$data, "reference") ||
            #     !rlang::has_name(works$data$reference[[1]], "DOI")) {
            #     refs_i <- tibble::tibble(title = dois$title[i],
            #                              doi = "",
            #                              update_date = Sys.Date())
            # } else {
            #     refs_i <- tibble::tibble(title = dois$title[i],
            #                              doi = works$data$reference[[1]]$DOI,
            #                              update_date = Sys.Date())
            # }

        }
        # new_refs <- new_refs |>
        #     dplyr::bind_rows() |>
        #     dplyr::filter(!is.na(.data$doi))
        # all_refs <- all_refs |>
        #     dplyr::bind_rows(new_refs) |>
        #     dplyr::distinct()
        # saveRDS(all_refs, out_file_ref)


        new_authors <- new_authors |>
            dplyr::bind_rows()
        all_authors <- all_authors |>
            dplyr::bind_rows(new_authors) |>
            dplyr::distinct()
        saveRDS(all_authors, out_file_author)
    }
#
#     if (!all) {
#         if (length(new_refs) == 0) {
#             return(invisible())
#         }
#         updated_refs <- all_refs |>
#             dplyr::filter(.data$doi %in% dois$doi) |>
#             dplyr::bind_rows(new_refs) |>
#             dplyr::pull(.data$title) |>
#             unique()
#         all_refs <- all_refs |>
#             dplyr::filter(.data$title %in% updated_refs)
#     }
#     refs <- all_refs |>
#         dplyr::left_join(all_dois |>
#                              dplyr::rename(reference = "title"), by = "doi") |>
#         dplyr::filter(!is.na(.data$reference)) |>
#         dplyr::select("title", "reference") |>
#         dplyr::distinct() |>
#         dplyr::filter(.data$title %in% all_dois$title)

    # Existing entry to cite this one

    return(invisible())
}




scopus_eid <- function() {

    dois <- get_dois(filter = "[tag[bibtex-entry]!hasp[draft.of]!is[system]has[bibtex-doi]!tag[Accepted Article]!has:field[scopus-eid]]")

    out_folder <- file.path(tws_options()$output, "scopus_pub")
    if (!dir.exists(out_folder)) {
        dir.create(out_folder, recursive = TRUE)
    }
    if (nrow(dois) == 0) {
        return(invisible())
    }
    max_request <- 4000
    request_num <- 0

    i <- 1
    for (i in seq(along = dois[[1]])) {
        tryCatch({

            message("Get EID from scopus for doi ", dois$doi[i], " with ", dois$title[i])
            out_file <- file.path(out_folder, paste0(dois$title[i], ".Rds"))
            if (file.exists(out_file)) {
                works <- readRDS(out_file)
            } else {
                works <- rscopus::embase_retrieval(id = dois$doi[i], identifier = "doi", verbose = FALSE)
                saveRDS(works, out_file)
            }
            if (length(works$content) == 0 || httr::status_code(works$get_statement) == 404) {
                eid <- ""
            } else {
                eid <- works$content$`abstracts-retrieval-response`$coredata$eid
            }
            rtiddlywiki::put_tiddler(dois$title[i], fields = list(`scopus-eid` = eid))
        }, error = function(e) {
            stop(e)
        })
    }
    return(invisible())
}
