# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:40 PM Saturday, 09 June 2018
# * Copyright: AS IS



# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
TWS_OPTIONS <- settings::options_manager(
    host = "http://127.0.0.1:8080/",
    output = "output",
    author_max = 10,
    file_expired = 90,
    file_remove_max = 3
)


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#'  host: host of tiddlywiki
#'  output: output for intermediate files
#'  file_expired: days to intermediate files expired
#'  file_remove_max: maximum number of intermediate file to remove
#'
#' @return the default and modified options.
#' @export
#' @examples
#' tws_options(host = "http://127.0.0.1:8080/")
tws_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    TWS_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @return the default options
#' @export
#' @examples
#' tws_reset()
tws_reset <- function() {
    settings::reset(TWS_OPTIONS)
}
