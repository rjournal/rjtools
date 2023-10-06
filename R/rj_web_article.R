#' @export
#' @rdname rjournal_article
rjournal_web_article <- function(toc = FALSE, self_contained = FALSE, ...) {
  base_format <- rjournal_article(
    self_contained = self_contained, toc = toc, ...
  )
  base_format$on_exit <- function() {}
  base_format
}
