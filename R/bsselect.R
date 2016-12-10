#' bsselect
#'
#' Function to generate a drop-down menu with the bootstrap-select jQuery plugin in an R Markdown document
#'
#' @param vector A named vector of values to send to the bootstrap-select dropdown menu.
#' @param selected The selected option; currently disabled
#' @param type One of \code{"text"}, \code{"img"}, or \code{"iframe"}.
#' @param frame_height The height of the image or iframe.  Defaults to 500px.
#' @param frame_width The width of the image or iframe.
#' @param align_right Whether to align the menu to the right instead of the left.  Defaults to FALSE.
#' @param dropup_auto Creates a drop-up menu automatically if there is not enough space to drop down.  Defaults to TRUE.
#' @param header Adds a header to the top of the menu.
#' @param live_search When TRUE, adds a search box to the drop-down menu.  Defaults to FALSE.
#' @param box_width One of "auto", "fit", a css-width in pixels, or FALSE.
#' @param live_search_style One of "contains" (the default) or "startsWith".
#' @param show_tick Whether to show a check mark next to the selected option.  Defaults to FALSE.
#' @param size The number of items to show in the window.  Defaults to "auto"; if FALSE, shows every option.
#' @param style Add the value to the button's style.  Options include "btn-primary", "btn-info", "btn-success", "btn-warning", and "btn-danger".
#' @param width The width of the htmlwidget.
#' @param height The height of the htmlwidget.
#' @param elementId The element ID.
#'
#' @seealso \url{https://silviomoreto.github.io/bootstrap-select/}
#'
#' @import htmlwidgets htmltools
#'
#' @export
bsselect <- function(vector, selected = NULL,
                     type = c("text", "img", "iframe"),
                     frame_height = "500", frame_width = "100%", align_right = FALSE,
                     dropup_auto = TRUE, header = FALSE, live_search = FALSE, box_width = FALSE,
                     live_search_style = "contains", show_tick = FALSE, size = "auto",
                     style = NULL, width = NULL, height = NULL, elementId = NULL, ...)  {


  # forward options using opts
  opts = list(
  )


  # create widget
  widg <- htmlwidgets::createWidget(
    name = 'bsselect',
    x = opts,
    width = width,
    height = height,
    package = 'bsselectR',
    elementId = elementId
  )

  # Prepend the HTML content to the widget

  out <- htmlwidgets::prependContent(widg, buildHTML(choices = vector, type = type,
                                                     dropdownAlignRight = align_right,
                                                     dropupAuto = dropup_auto,
                                                     header = header,
                                                     liveSearch = live_search,
                                                     liveSearchStyle = live_search_style,
                                                     showTick = show_tick,
                                                     size = size,
                                                     style = style,
                                                     height = frame_height, width = frame_width
                                                     ))

  out

}

#' Shiny bindings for bsselect
#'
#' Output and render functions for using bsselect within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a bsselect
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name bsselectR-shiny
#'
#' @export
bsselectOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'bsselect', width, height, package = 'bsselectR')
}

#' @rdname bsselectR-shiny
#' @export
renderBsselect <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, bsselectOutput, env, quoted = TRUE)
}

#' @keywords internal
bsselect_html <- function(id, style, class, ...){
  htmltools::attachDependencies(
    htmltools::tagList(
      tags$div(id=id, style=style, class=class, ...)
    ),
    value = list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap("default")
  )
)}

