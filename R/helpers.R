#' @import htmltools
#' @note code borrowed here from Shiny's selectInput
selectOptions <- function(choices, selected = NULL) {
  if (is.null(names(choices))) choices <- setNames(choices, choices)

  html <- mapply(choices, names(choices), FUN = function(choice, label) {
    sprintf(
      '<option value="%s"%s>%s</option>',
      htmlEscape(choice, TRUE),
      if (label %in% selected) ' selected' else '',
      htmlEscape(label)
    )
  }
  )

  HTML(paste(html, collapse = '\n'))
}


#' @import htmltools
buildHTML <- function(choices, selected = NULL, type = c("text", "img", "iframe"),
                      height = "500", width = "100%", dropdownAlignRight = FALSE,
                      dropupAuto = TRUE, header = FALSE, liveSearch = FALSE, boxWidth = FALSE,
                      liveSearchStyle = c("contains", "startsWith"), showTick = FALSE, size = "auto",
                      style = NULL) {

  id1 <- stringi::stri_rand_strings(1, 10)

  id2 <- stringi::stri_rand_strings(1, 10)

  l <- function(x) return(tolower(as.character(x)))

  select_tag <- tags$select(
    id = id1,
    class = "selectpicker",
    `data-dropdown-align-right` = l(dropdownAlignRight),
    `data-dropup-auto` = l(dropupAuto),
    `data-header` = l(header),
    `data-live-search` = l(liveSearch),
    `data-live-search-style` = liveSearchStyle,
    `data-show-tick` = l(showTick),
    `data-width` = l(boxWidth),
    `data-size` = l(size),
    `data-style` = style,
    selectOptions(choices, selected)
  )

  if (is.null(selected)) {
    sel <- choices[1]
  } else {
    index <- match(selected, names(choices))
    sel <- choices[index]
  }

  if (type == "text") {
    js <- paste0('$(document).ready(function(){
                  $("#', id1, '").change(function(){
                  $("#', id2, '").text($(this).val());
                  });
});')

    out <- tags$html(select_tag,
               p(sel,
                 id = id2),
               tags$script(htmlwidgets::JS(js)))

    return(renderTags(out, indent = FALSE))

  } else if (type == "img") {
    js <- paste0('$(document).ready(function(){
                 $("#', id1, '").change(function(){
                 $("img[name=', id2, ']").attr("src",$(this).val());

                 });

  });')
    out <- tags$html(select_tag,
               img(src = sel,
                   name = id2,
                   height = as.character(height),
                   width = as.character(width)),
               tags$script(htmlwidgets::JS(js)))
    return(renderTags(out, indent = FALSE))
    } else if (type == "iframe") {
      js <- paste0('$(document).ready(function(){
                   $("#', id1, '").change(function(){
                   $("#', id2, '").attr("src",$(this).val());

                   });

    });')

      out <- tags$html(select_tag,
                 tags$iframe(src = sel,
                             frameborder = "0",
                             height = height,
                             width = width,
                             id = id2),
                 tags$script(htmlwidgets::JS(js)))
      return(renderTags(out, indent = FALSE))

      }
  }
