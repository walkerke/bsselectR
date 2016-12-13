#' Embed bsselect dropdowns in Rmd documents as iframes
#'
#' @param widget The bsselect htmlwidget object you want to embed
#' @param file The name of the file you'd like to save to
#' @param height The height of the iframe
#' @param width The width of the iframe
#'
#' @export
as_iframe <- function(widget, file = NULL, height = "500", width = "100%"){

  if (is.null(file)) {
    file <- paste0(stringi::stri_rand_strings(1, 5), ".html")
  }

  htmlwidgets::saveWidget(widget = widget, file = file)

  out_iframe <- htmltools::tags$iframe(
                        src = file,
                        height = height,
                        width = width,
                        frameborder = "0",
                        scrolling = "no"
                        )

  out_iframe

}