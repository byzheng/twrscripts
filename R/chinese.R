
#' Convert tiddlers with Chinese title into pinyin
#'
#' @details
#' The converted pinyin is merged into field `aka`. An extra field `py-checked` is added to indicate
#' this tiddler is checked and will skip in the future.
#'
#' @return No return values
#' @export
convert_pinyin <- function() {
    mypy <- pinyin::pydic(method = 'toneless', dic = c("pinyin2"))
    all_tiddlers <- rtiddlywiki::get_tiddlers("[all[tiddlers]!search:title:regexp[\\w+]] +[!has:field[py-checked]]")

    i <- 1
    for (i in seq(along = all_tiddlers)) {
        if (!grepl("[a-zA-Z _]", all_tiddlers[[i]]$title)) {
            if (!is.null(all_tiddlers[[i]]$`py-checked`)) {
                message("Skip for ", all_tiddlers[[i]]$title)
                next
            }
            pinyin_i <- pinyin::py(all_tiddlers[[i]]$title, dic = mypy, sep = "")

            message("Converting ", all_tiddlers[[i]]$title, " to ", pinyin_i, " in field aka.")
            akas <- unique(c(pinyin_i,
                             rtiddlywiki::split_field(all_tiddlers[[i]]$aka)))
            #akas <- pinyin_i
            rtiddlywiki::put_tiddler(all_tiddlers[[i]]$title,
                                     fields = list(aka = akas,
                                                   `py-checked`=""))
        }
    }
    return(invisible())
}
