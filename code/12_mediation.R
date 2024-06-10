# libraries ----
source(here::here('code/setup.R'))
library(mediation)

# helper fns ----
extract_acme <- function(x){
  tibble(
    term = x$mediator,
    d0 = x$d0,
    d0.ci_low = x$d0.ci[1],
    d0.ci_high = x$d0.ci[2],
  )
}

extract_ade <- function(x) {
  tibble(
    term = x$mediator,
    z0 = x$z0,
    z0.ci_low  = x$z0.ci[1],
    z0.ci_high = x$z0.ci[2]
  )
}

# data ----
if (!fs::file_exists(here('data/mediation_data.rds'))) {
  ## load asset data
  load(here::here('data', 'commenter_assets.Rdata'), temp_env <- new.env())
  commenter_assets <- get('commenter_assets', envir = temp_env)
  commenter_assets$org_type <- commenter_assets$org_type2

  ## load efficacy data
  attachments <- read_csv(here::here('Data', 'attachments.csv'))

  # efficacy data
  efficacy_raw <- here::here('data', 'new_attachment_efficacy_w_docket.csv') %>%
    str_remove('finreg/') %>%
    read_csv()

  # merge in comment url from attachments table
  # drop empty url
  efficacy_raw <- efficacy_raw %>%
    dplyr::select(-comment_url)

  # JOINING LOOKUP TABLE DOES MUCH BETTER THAN USING URL VAR
  efficacy_raw <- efficacy_raw %>%
    left_join(attachments, by = join_by(attachment_url), relationship = "many-to-many") %>%
    distinct()

  # select needed vars. URL is comment URL (even for attachments)
  efficacy <- efficacy_raw %>%
    # make comment URL
    mutate(efficacy = cumulative_sequence_length_over_10 %>% as.numeric()) %>%
    dplyr::distinct(comment_url, efficacy)

  # dropping 17k
  efficacy <- efficacy %>%
    drop_na(efficacy)

  # take most efficacious attachment per comment
  efficacy <- efficacy %>%
    group_by(comment_url) %>%
    slice_max(order_by = efficacy) %>%
    ungroup()

  commenter_assets <- commenter_assets %>%
    mutate(comment_url = str_replace(comment_url, '\\?', 'QUESTIONMARK')) %>%
    left_join(
      efficacy %>%
        mutate(comment_url = str_replace(comment_url, '\\?', 'QUESTIONMARK')),
      by = join_by(comment_url)
    )

  # transform back
  commenter_assets <- commenter_assets %>%
    mutate(comment_url = comment_url %>%
             str_replace('QUESTIONMARK', '\\?'))

  #####
  ## load tech terms and bluebook
  load(here::here('Data', 'Dictionary_Terms.Rdata') %>% str_remove('finreg/'), temp_env <- new.env())
  bluebook_comments <- get('bluebook_comments', envir = temp_env) %>%
    drop_na(Total_Legal_Citations) %>%
    group_by(comment_url) %>%
    dplyr::select(-comment) %>%
    summarize(across(everything(), base::max))

  # whereas these should not be unique to comment URL
  bluebook_attachments <- get('bluebook_attachments', envir = temp_env) %>%
    drop_na(Total_Legal_Citations) %>%
    group_by(comment_url) %>%
    dplyr::select(-comment) %>%
    summarize(across(everything(), base::max))

  # join both and max again
  bluebook <- full_join(
    bluebook_attachments,
    bluebook_comments,
    by = join_by(
      comment_url, attachment_url, US_Code, Supreme_Court_Cases,
      Appeals_and_District_Court_Cases, Code_of_Federal_Regulations,
      Federal_Register_Total,
      Total_Legal_Citations
    )
  ) %>%
    group_by(comment_url) %>%
    summarize(across(everything(), base::max))

  # sum by comment URL
  technical_terms_comments <- get('technical_terms_comments', envir = temp_env) %>%
    drop_na(dictionary_terms) %>%
    group_by(comment_url) %>%
    dplyr::select(-comment) %>%
    summarize(across(everything(), base::max))

  # whereas these should not be unique to comment URL
  technical_terms_attachments <- get('technical_terms_attachments', envir = temp_env) %>%
    drop_na(dictionary_terms) %>%
    group_by(comment_url) %>%
    dplyr::select(-comment) %>%
    summarize(across(everything(), base::max))

  # join both and sum again
  technical_terms <- full_join(
    technical_terms_attachments,
    technical_terms_comments,
    by = join_by(
      comment_url, attachment_url, uk_law, banking, us_law, overlap,
      dictionary_terms
    )
  ) %>%
    group_by(comment_url) %>%
    summarize(across(everything(), base::max))


  # get from commenter_assets and bluebook + technical terms to commenter_assets to be
  commenter_assets <- commenter_assets %>%
    left_join(bluebook %>% dplyr::select(comment_url, Total_Legal_Citations), by = 'comment_url') %>%
    left_join(technical_terms %>% dplyr::select(comment_url, dictionary_terms), by = 'comment_url')

  commenter_assets <- commenter_assets %>%
    filter(!is.na(dictionary_terms))
  commenter_assets <- commenter_assets %>%
    mutate(
      assets_m = assets / 1000000,
      TotalLobbyingAmountNo0 = replace_na(MeanLobbyingAmount, 0) / 1000,
      MeanContribAmountNo0 = replace_na(MeanContribAmount, 0) / 1000,
      TotalLobbyingAmountNo0 = replace_na(TotalLobbyingAmount, 0) / 1000,
      TotalContribAmountNo0 = replace_na(TotalContribAmount, 0) / 1000
    )

  commenter_assets <- commenter_assets %>%
    mutate(marketcap_b = marketcap2 / 1000000000)

  # mediation
  mediation_data <- commenter_assets %>%
    drop_na(
      efficacy,
      marketcap2
    )
  saveRDS(mediation_data, here('data/mediation_data.rds'), compress = 'xz')
} else {
  mediation_data <- readRDS(here('data/mediation_data.rds'))
}

mediation_data4 <- mediation_data %>%
  drop_na(
    efficacy,
    marketcap_b,
    TotalLobbyingAmount,
    TotalContribAmount,
    dictionary_terms,
    Total_Legal_Citations
  )

run_old <- FALSE
if (run_old) {
  model_m4_cont <- lm(TotalContribAmount ~ marketcap_b,
                      data = mediation_data4
  )

  model_m4_lob <- lm(TotalLobbyingAmount ~ marketcap_b,
                     data = mediation_data4
  )

  model_m4_terms <- lm(dictionary_terms ~ marketcap_b,
                       data = mediation_data4
  )

  model_m4_legal <- lm(Total_Legal_Citations ~ marketcap_b,
                       data = mediation_data4
  )


  model_y4 <- glm(
    efficacy ~ marketcap_b + TotalLobbyingAmount + TotalContribAmount + dictionary_terms + Total_Legal_Citations,
    data = mediation_data4
  )

  med_cont_4 <- mediate(model_m4_cont, model_y4,
                        sims = 1000, treat = 'marketcap_b',
                        mediator = 'TotalContribAmount',
                        robustSE = TRUE, boot = TRUE,
                        control.value = 1, treat.value = 2
  )

  med_lob_4 <- mediate(model_m4_lob, model_y4,
                       sims = 1000, treat = 'marketcap_b',
                       mediator = 'TotalLobbyingAmount',
                       robustSE = TRUE, boot = TRUE,
                       control.value = 1, treat.value = 2
  )

  med_terms_4 <- mediate(model_m4_terms, model_y4,
                         sims = 1000, treat = 'marketcap_b',
                         mediator = 'dictionary_terms',
                         robustSE = TRUE, boot = TRUE,
                         control.value = 1, treat.value = 2
  )

  med_legal_4 <- mediate(model_m4_legal, model_y4,
                         sims = 1000, treat = 'marketcap_b',
                         mediator = 'Total_Legal_Citations',
                         robustSE = TRUE, boot = TRUE,
                         control.value = 1, treat.value = 2
  )



  models_m4 <- list(model_m4_cont, model_m4_lob, model_m4_terms, model_m4_legal, model_y4)

  rows <- tibble(
    term = c('Dependent Variable'),
    `1` = 'Campaign (PAC) Spending',
    `2` = 'Lobbying (LDA) Spending',
    `3` = 'Technical Terms',
    `4` = 'Legal Citations',
    `5` = 'Efficacy'
  )

  attr(rows, 'position') <- c(0)

  modelsummary(models_m4, rows = rows)


  plot(med_terms_4,
       main = 'Technical Terms',
       xlab = 'Words from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization'
  )

  plot(med_legal_4,
       main = 'Legal Citations',
       xlab = 'Words from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization'
  )

  plot(med_cont_4,
       main = 'Political Spending',
       xlab = 'Words from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization'
  )

  plot(med_lob_4,
       main = 'Lobbying Expenditure',
       xlab = 'Words from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization'
  )

}

mediation_data_comb <- mediation_data %>%
  drop_na(
    efficacy,
    marketcap_b,
    TotalLobbyingAmount,
    TotalContribAmount,
    dictionary_terms,
    Total_Legal_Citations
  )

mediation_data_comb <- mediation_data_comb %>%
  mutate(
    political_spending = TotalContribAmount + TotalLobbyingAmount
  )

model_comb_poli <- lm(political_spending ~ marketcap_b,
                      data = mediation_data_comb
)

model_comb_terms <- lm(dictionary_terms ~ marketcap_b,
                     data = mediation_data_comb
)

model_y_comb <- glm(
  efficacy ~ marketcap_b + political_spending + dictionary_terms,
  data = mediation_data_comb
)

# arent the models pretty?

models_comb <- list(model_comb_poli, model_comb_terms, model_y_comb)
rows <- tibble(
  term = c('Dependent Variable'),
  `1` = 'Political (PAC + LDA) Spending',
  `2` = 'Technical Terms',
  `3` = 'Efficacy'
)

attr(rows, 'position') <- c(0)

modelsummary(models_comb, rows = rows)

# mediation ====
# generic mediation, one var
med_comb <- mediate(model_comb_poli, model_y_comb,
                    sims = 1000, treat = 'marketcap_b',
                    mediator = 'political_spending',
                    robustSE = TRUE, boot = TRUE,
                    control.value = 1, treat.value = 2
)

plot(med_comb,
     main = 'Political Spending',
     xlab = 'Words from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization'
)

# multiple mediators
set.seed(02138)
med_multi_comb <- multimed(
  outcome = 'efficacy',
  med.main = 'political_spending',
  med.alt = 'dictionary_terms',
  treat = 'marketcap_b',
  data = mediation_data_comb,
  sims = 5000
)
summary(med_multi_comb)
plot(med_multi_comb, tg = "av")

# Alternative multiple mediators using `mediations()`
outcome <- 'efficacy'
mediators <- c('TotalContribAmount', 'TotalLobbyingAmount', 'dictionary_terms', 'Total_Legal_Citations')
treatment <- 'marketcap_b'
datasets <- list(a = mediation_data_comb) # needs to be named see issue https://github.com/kosukeimai/mediation/issues/16

meds <- mediations(
  datasets = datasets, treatment = treatment, mediators = mediators,
  outcome = outcome, #covariates = paste(mediators, collapse = ' + '),
  families = c('gaussian', 'gaussian'), interaction = FALSE,
  conf.level = .95, sims = 1000,
  control.value = 1, treat.value = 2
)

acme <- purrr::map_df(meds, extract_acme)

acme %>%
  mutate(
    term = case_match(term,
                      "TotalContribAmount" ~ "Campaign (PAC) Spending",
                      "TotalLobbyingAmount" ~ "Lobbying (LDA) Spending",
                      "Total_Legal_Citations" ~ " Legal Citations",
                      "dictionary_terms" ~ " Technical Sophistication")
  )%>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey") +
  geom_pointrange(aes(x = d0, xmin = d0.ci_low, xmax = d0.ci_high,
                      y = term)) +
  labs(y = "Mediator", x = "ACME\nWords from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization") +
  theme(panel.grid.major.y = element_blank())
ggsave(here('figs/multiple_mediators_acme.png'), dpi = 300, width = 6, height = 3)

ade <- purrr::map_df(meds, extract_ade)

ade %>%
  mutate(
    term = case_match(term,
                      "TotalContribAmount" ~ "Campaign (PAC) Spending",
                      "TotalLobbyingAmount" ~ "Lobbying (LDA) Spending",
                      "Total_Legal_Citations" ~ " Legal Citations",
                      "dictionary_terms" ~ " Technical Sophistication")
  ) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey") +
  geom_pointrange(aes(x = z0, xmin = z0.ci_low, xmax = z0.ci_high,
                      y = term)) +
  labs(y = "Mediator", x = "ADE\nWords from Comment Added to Final Rule\nper Billion Dollars in Market Capitalization") +
  theme(panel.grid.major.y = element_blank())
