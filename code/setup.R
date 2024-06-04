
requires <- c("bookdown",
              "xaringan",
              "xaringanthemer",
              "fixest",
              "flextable",
              "marginaleffects",
              "latex2exp",
              "ggthemes",
              "RSQLite",
              "english",
              "cowplot", #FIXME not sure where this is called but modelsummary seems to want it?
              "maps",
              "scales",
              "magrittr",
              "readxl",
              #"broom",
              "here",
              "ggridges",
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
              #"equatiomatic",
              "DiagrammeR",
              #"tidytext",
              #"latex2exp",
              "tidyverse")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
rm(requires, to_install)

# if(!"equatiomatic" %in% rownames(installed.packages())){
#   remotes::install_github("datalorax/equatiomatic")
# }

library(bookdown)
library(fixest)
library(flextable)
library(ggthemes)
library(scales)
library(magrittr)
library(readxl)
# library(maps) # don't load library because it replaces purrr:map()
library(here)
library(knitr)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(mediation)
#library(lme4)
#library(lmerTest)
library(modelsummary)
library(marginaleffects)
library(fixest)
library(tidyverse)

# slides
library("xaringan")
library("xaringanthemer")



## Sets defaults for R chunks
knitr::opts_chunk$set(echo = TRUE, # echo = TRUE means that code will show
                      cache = FALSE,
                      #cache = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.show="hold",
                      fig.pos= "htbp",
                      fig.path = "figs/",
                      fig.align='center',
                      fig.cap = '   ',
                      fig.retina = 3,
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
  #if(knitr:::is_html_output() | knitr::is_latex_output() ){
  x %>%
    row_spec(row = 1,
             bold = T,
             hline_after = TRUE
             ) %>%
      kable_styling(
        font_size = 9,
        full_width = TRUE,
        latex_options = c("repeat_header")
        )
  # } else{
  #   kableExtra::as_image(x, width = 6.5, file = paste0("figs/", file, ".png"))
  #   }
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
       "ASSETS_m" = "Assets (Millions)",
       "ASSETS_b" = "Assets (Billions)",
       "log(ASSETS_b)" = "Log(Assets)",
       "ASSETS" = "Assets (Hundreds of Millions)",
       "Assets" = "Assets (Billions)",
       "assets" = "Assets",
       "assets_t" = "Assets (in Thousands)",
       "assets_m" = "Assets (in Millions)",
       "assets_b" = "Assets (in Billions)",
       "assets_b2" = "Assets Squared",
       "log(assets_b + 1)" = "Log(Assets)",
       "log(assets_b + 1):org_typeCredit union" = "Log(Assets) x Credit Union",
       "org_typeCredit union:assets_b2" = "Assets Squared x Credit Union",
       "log(assets_b + 1):org_typeIndustry assoc." = "Log(Assets) x Industry assoc.",
       "assets_b:org_typeIndustry assoc." = "Assets x Industry assoc.",
       "org_typeIndustry assoc.:assets_b2" = "Assets Squared x Industry assoc.",
       "log(assets_b + 1):org_typeOther non-profit" = "Log(Assets) x Other non-profit",
       "assets_b:org_typeOther non-profit" = "Assets x Other non-profit",
       "org_typeOther non-profit:assets_b2" = "Assets Squared x Other non-profit",
       "log(assets_b + 1):org_typeNon-profit" = "Log(Assets) x Non-profit",
       "org_typeNon-profit:assets_b2" = "Assets Squared x Non-profit",
       "marketcap_b" = "Market Capitalization\n(Billions)",
       "log(marketcap_b)" = "Log(Market Capitalization)",
       "donations_m" = "Campaign Donations (Millions)",
       "MeanContribAmount_t" = "Avg. PAC Spending (Thousands/Year)",
       "TotalContribAmount_t" = "Total PAC Spending (Thousands)",
       "log(MeanContribAmount_t)" = "Log(Avg. PAC Spending)",
       "log(TotalContribAmount_t)" = "Log(Total PAC Spending)",
       "onepercentTop 1%" = "Top 1% Most Frequent",
       "Class" = "Bank Class",
       "Class2" = "Bank Type",
       "Class2Non-commercial bank" = "Non-commercial bank",
       "ASSETS_b:Class2Non-commercial bank"= "Assets x Non-commercial bank",
       "ASSETS_m:Class2Non-commercial bank"  = "Assets x Non-commercial bank",
       "log(ASSETS_b):Class2Non-commercial bank" = "Log(Assets) x Non-commercial bank",
       "Class SM" = "State Commercial Bank",
       "Class SB" = "Savings Bank",
       "Class SA" = "Savings Association",
       "Class NM" = "Commercial Bank",
       "Class N" = "National Bank",
       "ASSETS:Class SM" = "Assets x State Bank",
       "ASSETS:Class SB" = "Assets x Savings Bank",
       "ASSETS:Class SA" = "Assets x Savings Association",
       "ASSETS:Class NM" = "Assets x Commercial Bank",
       "org_typeCredit union" = "Credit union",
       "org_typeNon-profit" = "Non-profit",
       "org_typeIndustry assoc." = "Industry assoc.",
       "org_typeOther non-profit" = "Other non-profit",
       "assets_b:org_typeCredit union" = "Assets x Credit union",
       "assets_b:org_typeNon-profit" = "Assets x Non-profit",
       "marketcap2" =  "Market Capitalization",
       "dictionary_terms" = "Technical Terms",
       "Total_Legal_Citations" = "Legal Citations",
       "log(dictionary_terms + 1)" = "Log(Technical Terms)",
       "log(Total_Legal_Citations + 1)" = "Log(Legal Citations)",
       "MeanContribAmount" = "PAC Donations",
       "TotalLobbyingAmount" = "Lobbying",
       "TotalContribAmount" = "PAC Donations",
       "TotalContribAmountNo0" = "PAC Spending",
       "TotalLobbyingAmountNo0" = "Lobbying Spending",
       "ind_assocTRUE" = "Industry Assoc.")

gm = list("Num.Obs." = "Number of Comments",
          "R2" = "R2",
          "AIC" = "AIC")

# table  defaults
modelsummary <- function(...){
  modelsummary::modelsummary(...,
                             stars = T,
                             add_rows = rows,
                             coef_rename = cm,
                             gof_omit = "R.*|A.*|B.*",
                             coef_omit = "(Intercept)")# %>%
    #kableExtra::row_spec(row = 1, bold = T)
}

# coeficient plot defaults
modelplot <- function(...) modelsummary::modelplot(..., coef_omit = "(Intercept)", coef_map = cm) +
  geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
  aes(shape = model)


# style_mono_light(base_color = "#000000", # "#F2DFCE",
#                  link_color	 = "#0D7680",
#                  inverse_link_color	 = "#FFCFB3",
#                  background_color = "#FFF1E0",
#                  header_background_content_padding_top = "7rem",
#                  table_row_even_background_color = "#FFCFB3",
#                  header_font_google = google_font("Playfair Display"),
#                  text_font_google = google_font("Playfair Display"),
#                  base_font_size = "30px",
#                  text_font_size = "1rem",
#                  code_font_size = "1rem",
#                  header_h1_font_size = "1.5rem",
#                  header_h2_font_size = "1.25rem",
#                  header_h3_font_size = "1.25rem",
#                  padding = "25px",
#                  code_font_google = google_font("Inconsolata")
# )

options(scipen=999)




densityplot <- function(data,
                        var = "ASSET",
                        title = "Banks",
                        by = 1000000,
                        x = "Assets (Millions)",
                        caption = "",
                        fill = "",
                        y = ""){
  data$var <- data %>% pull(var)
  data$by <- by

  data %>%
    group_by(Commented) %>%
    mutate(mean = mean(var, na.rm =TRUE),
           median = median(var, na.rm =TRUE)) %>%
    group_by(Commented) %>%
    mutate(Commented = Commented %>% paste0(
      "\nmedian = $", round(median, 0) %>% prettyNum(big.mark = ",") ,
      "\nmean = $", round(mean, 0) %>% prettyNum(big.mark = ",")#, "\n"
      )) %>%
    ggplot() +
    aes(x = var/by,
        fill = Commented, color = Commented) +
    #geom_vline( aes(xintercept = mean_ASSET/by, color = Commented)) +
    #geom_vline( aes(xintercept = median_ASSET/by, color = Commented), linetype = 2) +
    geom_density(alpha = .5, color = NA) +
    scale_x_log10() +
    labs(title = title,
         subtitle = str_c("N = ",
                          data %>% drop_na(var) %>% nrow() %>% pretty_num()
                          ),
         fill = fill, y = y,
         x = x,
         caption = caption)+
    scale_fill_discrete(guide = guide_legend(reverse = TRUE) ) +
    scale_color_discrete(alpha = .5, guide = "none") +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0),
          axis.text.y = element_blank(),
          plot.title = element_text(vjust = -6),
          plot.subtitle = element_text(vjust = -6))
}

