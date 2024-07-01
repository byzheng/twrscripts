
#' Get works from all resources including ORCID, Scopus
#'
#' @param is_new Logical. Whether to only process new records.
#'
#' @return No return
#' @export
works <- function(is_new = TRUE) {

    message("ORCID")
    works_orcid(is_new = is_new)

    message("Scopus")
    works_scopus(is_new = is_new)

    message("Colleague Homepage")
    works_homepage(is_new = is_new)

    message("Researcher ID")
    works_wos(new = new)
    # message("Google Scholar")
    # with_scholar(new = new)
}
