# Messing around to try to get a skeleton right

library(htmltools)
library(stringr)

# Generate vector of PNG images

vec <- list.files(path = "D:/Users/kylewalker/Box Sync/Center for Urban Studies/bikeshare/dropdown/plots",
                  pattern = "png", full.names = TRUE)

names <- str_replace(basename(vec), "\\.png", "")

vec <- setNames(vec, names)

# Adapted from Shiny selectInput code
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

select_tag <- tags$select(
  id = stringi::stri_rand_strings(1, 10),
  class = "selectpicker"
)



selectOptions(vec)