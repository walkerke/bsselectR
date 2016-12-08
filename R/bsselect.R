#' bsselect
#'
#' Function to generate a drop-down menu
#'
#' @import htmlwidgets htmltools
#'
#' @export
bsselect <- function(vector, selected = NULL,
                     type = c("text", "img", "iframe"),
                     frame_height = "500", frame_width = "100%", actions_box = FALSE, align_right = FALSE,
                     dropup_auto = TRUE, header = FALSE, live_search = FALSE, box_width = FALSE,
                     live_search_style = "contains", show_tick = FALSE, width = NULL,
                     height = NULL, elementId = NULL, ...)  {


  # forward options using opts
  opts = list(
  )

  deps <- list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap("default")
  )

  # create widget
  widg <- htmlwidgets::createWidget(
    name = 'bsselect',
    x = opts,
    width = width,
    height = height,
    package = 'bsselectR',
    elementId = elementId,
    dependencies = deps
  )

  # Prepend the HTML content to the widget

  out <- htmlwidgets::prependContent(widg, buildHTML(choices = vector, type = type, actionsBox = actions_box,
                                                     dropdownAlignRight = align_right,
                                                     dropupAuto = dropup_auto,
                                                     header = header,
                                                     liveSearch = live_search,
                                                     liveSearchStyle = live_search_style,
                                                     showTick = show_tick,
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

#' importFrom htmltools htmlEscape HTML
selectOptions <- function(choices, selected = NULL) {
  if (is.null(names(choices))) choices <- setNames(choices, choices)

  html <- mapply(choices, names(choices), FUN = function(choice, label) {
    sprintf(
      '<option value="%s"%s>%s</option>',
      htmlEscape(choice, TRUE),
      if (choice %in% selected) ' selected' else '',
      htmlEscape(label)
    )
  }
  )

  HTML(paste(html, collapse = '\n'))
}


#' importFrom htmltools tags div img
buildHTML <- function(choices, selected = NULL, type = c("text", "img", "iframe"),
                      height = "500", width = "100%", actionsBox = FALSE, dropdownAlignRight = FALSE,
                      dropupAuto = TRUE, header = FALSE, liveSearch = FALSE, boxWidth = FALSE,
                      liveSearchStyle = "contains", showTick = FALSE) {

  id1 <- stringi::stri_rand_strings(1, 10)

  id2 <- stringi::stri_rand_strings(1, 10)

  l <- function(x) return(tolower(as.character(x)))

  select_tag <- tags$select(
    id = id1,
    class = "selectpicker",
    `data-actions-box` = l(actionsBox),
    `data-dropdown-align-right` = l(dropdownAlignRight),
    `data-dropup-auto` = l(dropupAuto),
    `data-header` = l(header),
    `data-live-search` = l(liveSearch),
    `data-live-search-style` = liveSearchStyle,
    `data-show-tick` = l(showTick),
    `data-width` = l(boxWidth),
    selectOptions(choices, selected)
  )

  if (type == "text") {
    suppressWarnings(return(select_tag))
  } else if (type == "img") {
    js <- paste0('$(document).ready(function(){
                 $("#', id1, '").change(function(){
                 $("img[name=', id2, ']").attr("src",$(this).val());

                 });

  });')
    out <- div(select_tag,
               img(src = choices[1],
                   name = id2,
                   height = as.character(height),
                   width = as.character(width)),
               tags$script(htmlwidgets::JS(js)))
    return(out)
    } else if (type == "iframe") {
      js <- paste0('$(document).ready(function(){
                   $("#', id1, '").change(function(){
                   $("#', id2, '").attr("src",$(this).val());

                   });

    });')

      out <- div(select_tag,
                 tags$iframe(src = choices[1],
                             frameborder = "0",
                             height = height,
                             width = width,
                             id = id2),
                 tags$script(htmlwidgets::JS(js)))
      return(out)

      }
  }
