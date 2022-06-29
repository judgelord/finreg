
requires <- c("bookdown",
              "scales",
              "magrittr",
              #"broom",
              "here",
              #"msm",
              "kableExtra",
              "modelsummary",
              #"dotwhisker",
              "mediation",
              #"lme4",
              #"lmerTest",
              #"fixest",
              #"flextable",
              #"magick",
              "equatiomatic",
              #"tidytext",
              #"latex2exp",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

library(scales)
library(magrittr)
#library(broom)
#library(dotwhisker)
library(here)
library(knitr)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
#library(mediation)
#library(lme4)
#library(lmerTest)
#library(fixest)
library(modelsummary)
library(tidyverse)

# slides
library("xaringan")
library("xaringanthemer")



## Sets defaults for R chunks
knitr::opts_chunk$set(echo = FALSE, # echo = TRUE means that code will show
                      cache = FALSE,
                      #cache = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.show="hold",
                      fig.pos= "htbp",
                      fig.path = "figs/",
                      fig.align='center',
                      fig.cap = '   ',
                      fig.retina = 1,
                      fig.height = 3,
                      fig.width = 7,
                      out.width = "100%",
                      out.extra = "")

library(tidyverse)
library(ggthemes)
library(latex2exp)
theme_set(theme_tufte())
theme_update(plot.title = element_text(hjust=0.5,size=12))

# defaults for plots
library(ggplot2); theme_set(theme_minimal());
options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_color_discrete <- function(...){
  scale_color_viridis_d(..., direction = -1,
                        begin = 0, end = .6, option = "plasma")}
scale_fill_discrete <- function(...){
  scale_fill_viridis_d(..., direction = -1,
                       begin = 0, end = .6, option = "plasma")}

scale_color_continuous <- function(...){
  scale_color_viridis_c(..., direction = -1,
                        option = "plasma")}
scale_fill_continuous <- function(...){
  scale_fill_viridis_c(..., direction = -1,
                       option = "plasma")}


# Table formatting

# Table formatting
library(kableExtra)
kablebox <- . %>%
  slice_head(n = 100) %>%
  knitr::kable() %>%
  kable_styling() %>%
  scroll_box(height = "400px")

# a function to format kables for different output formats
kable2 <- function(x, file){
  if(knitr:::is_html_output() | knitr::is_latex_output() ){
      x %>% row_spec(row = 1, bold = T, hline_after = TRUE) %>%
      kable_styling(font_size = 9, full_width = TRUE, latex_options = c("repeat_header"))
  } else{
    kableExtra::as_image(x, width = 6.5, file = paste0("figs/", file, ".png"))
    }
}


library(flextable)



# A function to trim and format tables for different outputs
kable3 <- function(x,
                   caption = "",
                   height = '400px',
                   full_width = F,
                   align = 'l',
                   font_size = 11,
                   latex_options = "repeat_header"){
  if(knitr:::is_html_output()) {
    x %>%
      ungroup() %>%
      slice_head(n = 100) %>%
      mutate(across(where(is.numeric), pretty_num)  ) %>%
      knitr::kable(caption = caption) %>%
      kable_styling() %>%
      scroll_box(height = height)
  } else{
    if(knitr::is_latex_output() ){
      x %>%
        ungroup() %>%
        slice_head(n = 20) %>%
        mutate(across(where(is.numeric), pretty_num)  ) %>%
        knitr::kable(format = "latex",# "pipe", # "latex"?
                     caption = caption,
                     booktabs = T,
                     align = align,
                     linesep = "\\addlinespace") %>%
        kable_styling(font_size = font_size,
                      full_width = full_width,
                      latex_options = latex_options)
    } else{x %>%
        ungroup() %>%
        slice_head(n = 20) %>%
        flextable() %>%
        set_caption(caption) %>%
        autofit() %>%
        fit_to_width(max_width = 6)
      #knitr::kable(caption = caption) %>%
      #kableExtra::as_image(file = paste0("figs", caption, ".png"))
    }
  }
}

options(scipen=999)


# smarter number functions
smart_number <- function(n, ...) {
  # if non-int below ten, return as is
  if ( (n != as.numeric(n)) ) {
    return(n)
  } else
    # if non-int above ten, return number()
    if (abs(n) >= 10 | as.numeric(n) != as.integer(n) ) {
      return( prettyNum(n, big.mark = ",") )#scales::number(n, big.mark = ",", ...))
    } else
      # if int below 10, print english
      if (abs(n) < 10 & as.numeric(n) == as.integer(n) ) {
        return(english::english(n, ...))
      } else
        stop("??")
}



# inline formatting
knit_hooks$set(inline = function(x) {
  if (is.na(as.numeric(x))) {
    return(x)
    } else
      x <- as.numeric(x)
      # omit years
    if (x > 2021 | x < 1980)  {
      return(smart_number(x))
      } else
        return(x) #prettyNum(x, big.mark = ",") #
})

# number formatting
pretty_num <- . %>% prettyNum(big.mark = ",")


options(stringsAsFactors = FALSE)

options(knitr.graphics.auto_pdf = TRUE)


#functions for case sensitive string manipulation
str_rm_all <- function(string, pattern) {
  str_remove_all(string, regex(pattern, ignore_case = TRUE))
}

str_rpl <- function(string, pattern, replacement) {
  str_replace(string, regex(pattern, ignore_case = TRUE), replacement)
}

str_rm <- function(string, pattern) {
  str_remove(string, regex(pattern, ignore_case = TRUE))
}

str_dct <- function(string, pattern) {
  str_detect(string, regex(pattern, ignore_case = TRUE))
}

str_ext <- function(string, pattern) {
  str_extract(string, regex(pattern, ignore_case = TRUE))
}

str_spl <- function(string, pattern) {
  str_split(string, regex(pattern, ignore_case = TRUE))
}


# var names
cm = c("ASSET" = "Assets",
       "ASSETS" = "Assets (Hundreds of Millions)",
       "Assets" = "Assets (Billions)",
       "assets" = "Assets",
       "assets_t" = "Assets (in Thousands)",
       "assets_m" = "Assets (in Millions)",
       "assets_b" = "Assets (in Billions)",
       "marketcap_b" = "Market Capitalization (Billions)",
       "donations_m" = "Campaign Donations (Millions)",
       "onepercentTop 1%" = "Top 1% Most Frequent",
       "Class SM" = "State Bank",
       "Class SB" = "Savings Bank",
       "Class SA" = "Savings Association",
       "Class NM" = "Commercial Bank",
       "ASSETS:Class SM" = "Assets x State Bank",
       "ASSETS:Class SB" = "Assets x Savings Bank",
       "ASSETS:Class SA" = "Assets x Savings Association",
       "ASSETS:Class NM" = "Assets x Commercial Bank",
       "org_typeCredit union" = "Credit union",
       "org_typeNon-profit" = "Non-profit",
       "assets_b:org_typeCredit union" = "Assets x Credit union",
       "assets_b:org_typeNon-profit" = "Assets x Non-profit")

# table  defaults
modelsummary <- function(...) modelsummary::modelsummary(...,
                                                         stars = T,
                                                         add_rows = rows,
                                                         coef_rename = cm,
                                                         gof_omit = "R.*|A.*|B.*",
                                                         coef_omit = "(Intercept)") %>%
  row_spec(row = 1, bold = T)

# coeficient plot defaults
modelplot <- function(...) modelsummary::modelplot(..., coef_omit = "(Intercept)") +
  geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
  aes(shape = model)


# SLIDE FORMATTING
style_mono_light(base_color = "#3b444b",
                 link_color = "#B7E4CF",
                 #background_color = "#FAF0E6", # linen
                 header_font_google = google_font("PT Sans"),
                 text_font_google = google_font("Old Standard"),
                 text_font_size = "18px",
                 padding = "10px",
                 code_font_google = google_font("Inconsolata"),
                 code_inline_background_color    = "#F5F5F5",
                 table_row_even_background_color = "#ddede5",
                 extra_css =
                   list(".remark-slide-number" = list("display" = "none")))

options(scipen=999)





