# bsselectR
Add bootstrap-select dropdown menus to your R Markdown documents without Shiny

To install: `devtools::install_github("walkerke/bsselectR")`

The `bsselect` function will generate an htmlwidget that allows you to interactively display text, images, and iframes from a named vector.  More documentation is coming soon; in the meantime, here is a preview of how the package works:  

```r
library(bsselectR)

quotes <- c("Look deep into nature, and then you will understand everything better.", 
            "A fool thinks himself to be wise, but a wise man knows himself to be a fool.", 
            "My mission in life is not merely to survive, but to thrive; and to do so with some passion, 
            some compassion, some humor, and some style.")

quotes <- setNames(quotes, c("Einstein", "Shakespeare", "Angelou"))

bsselect(quotes, type = "text", height = 200)
```

<img src="https://dl.dropbox.com/s/87t1jlbqphhj7or/quotes.gif">

You can even make your R Markdown presentations interactive!

<img src="https://dl.dropbox.com/s/a29672wl9f6m1uu/bikeshare1.gif">
