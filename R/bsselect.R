#' bsselect
#'
#' Function to generate a drop-down menu
#'
#' @import htmlwidgets htmltools
#'
#' @export
bsselect <- function(vector, selected = NULL,
                     type = c("url", "img", "iframe"),
                     height1 = "auto", width1 = "100%", actionsBox = FALSE, dropdownAlignRight = FALSE,
                     dropupAuto = TRUE, header = FALSE, liveSearch = FALSE,
                     liveSearchStyle = "contains", showTick = FALSE,
                     width = NULL, height = NULL, elementId = NULL)  {

  data_to_send <- buildHTML(choices = vector, type = type, height = height1, width = width1)

  # forward options using opts
  opts = list(
    data = data_to_send,
    actionsBox = actionsBox,
    dropdownAlignRight = dropdownAlignRight,
    dropupAuto = dropupAuto,
    header = header,
    liveSearch = liveSearch,
    liveSearchStyle = liveSearchStyle,
    showTick = showTick
  )

  deps <- list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap("default")
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'bsselect',
    opts,
    width = width,
    height = height,
    package = 'bsselectR',
    elementId = elementId,
    dependencies = deps
  )
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
buildHTML <- function(choices, selected = NULL, type = c("url", "img", "iframe"),
                      height = "auto", width = "100%") {

  id1 <- stringi::stri_rand_strings(1, 10)

  id2 <- stringi::stri_rand_strings(1, 10)

  select_tag <- tags$select(
    id = id1,
    class = "selectpicker",
    selectOptions(choices, selected)
  )

  if (type == "url") {
    return(select_tag)
  } else if (type == "img") {
    js <- paste0('$(document).ready(function(){
                 $("#', id1, '").change(function(){
                 $("img[name=', id2, ']").attr("src",$(this).val());

                 });

  });')
    out <- div(select_tag,
               img(src = choices[1],
                   name = id2,
                   height = height,
                   width = width),
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
