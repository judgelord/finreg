`%!in%` <- Negate(`%in%`) 

efficacy_raw %>%
  dplyr::anti_join(attachments, by = "comment_url")

which(efficacy_raw$attachment_url %!in% attachments$comment_url)

efficacy_raw %>%
  dplyr::anti_join(attachments, by = "attachment_url")

which(efficacy_raw$attachment_url %!in% attachments$attachment_url)

### Efficacy v Sophistication Plot ###
efficacyXsophistication %>% # filter(efficacy > 0) %>% #FIXME 
  ggplot() +
  aes(x = dictionary_terms, y = efficacy) + 
  geom_point(aes(y = 10131, x = 7705), color = "red", shape = 0, size = 3) +
  geom_point(alpha = .5) + 
  geom_smooth(method = "lm") +
  geom_smooth(color = "light blue") +
  labs(caption = str_c("Comments on Dodd-Frank Rules, N = ", nrow(efficacyXsophistication) %>% prettyNum(big.mark = ",")),
       y = "Lobbying Success\n(Words From Comment\nAdded to Final Rule)",
       x = "Technical Terms in Comment",
       color = "") +
  theme(legend.position = "bottom") + 
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() 

# 1 # BinScatter
efficacyXsophistication

bivariate_binscatter_manual <- function(long_formula, data, tiles){
  # long_formula <- "Total_Legal_Citations ~ MeanContribAmount"
  
  long_formula <- as.formula(long_formula)
  
  long_reg <- lm(long_formula,data = data)
  
  x_bins <- long_reg$model %>% tibble() %>%
    rename(x = 2, y = 1) %>%
    arrange(y, x) %>%
    mutate(
      x_bins = ntile(x, tiles)
    ) %>%
    group_by(x_bins) %>%
    mutate(
      x_mean_by_tile = round(mean(x), 1),
      y_mean_by_tile = round(mean(y),1),
      weight = n(),
    ) %>% 
    ungroup() %>% 
    dplyr::select(y_mean_by_tile, x_mean_by_tile, weight) %>% 
    distinct() %>%
    arrange(y_mean_by_tile, x_mean_by_tile)
  
  # this is a residualized binscatter
  return_plot <- ggplot(x_bins, aes(x = x_mean_by_tile, y = y_mean_by_tile, size = as.character(weight))) + 
    geom_point() + 
    geom_smooth(method = "lm")+
    #ggplot2::geom_abline(slope = short_reg_coefs$estimate[2], intercept = short_reg_coefs$estimate[1])
    theme_minimal() + theme(legend.position = "none")
  
  return(return_plot)
}

bivariate_binscatter_manual(long_formula = "log(efficacy + 1) ~ log(dictionary_terms + 1)", 
                            data = efficacyXsophistication %>% 
                              dplyr::filter(org == "Matched organization"), tiles = 100) + 
  ggplot2::scale_y_log10() +
  ggplot2::scale_x_log10()

summary(binsreg::binsregselect(y = efficacy, x = dictionary_terms, data = data.frame(efficacyXsophistication)))

binsreg::binsreg(y = efficacy, x = dictionary_terms, 
                 data = data.frame(efficacyXsophistication %>%
                                     mutate(
                                       efficacy = log(efficacy + 1),
                                       dictionary_terms = log(dictionary_terms + 1)
                                       )
                                   )
                 )

# create logged variable 
# do by quantiles 

#efficacy_noise <- runif(43506, 0, 10)

efficacyXsophistication %>% 
  dplyr::filter(org == "Matched organization") %>%
  #filter(efficacy > 0) %>%
  arrange(dictionary_terms) %>%
  mutate(
    log_efficacy = log10(efficacy + 1),
    log_dictionary_terms = log10(dictionary_terms + 1),
    log_dictionary_terms_quantiles = ntile(log_dictionary_terms, 10),
    group = as.factor(log_dictionary_terms_quantiles),
    #efficacy = ifelse(group %in% c(1,2,3), efficacy + jitter(efficacy), efficacy),
    #efficacy = ifelse(group == 3, efficacy + 1.1, efficacy),
    
    #bin=cut_width(log_dictionary_terms, width=1, boundary=0)
  ) %>% 
  filter(!is.na(group)) %>%
  ggplot(aes(x=dictionary_terms, y=efficacy)) +
  geom_point(aes(x=dictionary_terms, y=efficacy), alpha = 0.03) + 
  geom_vridgeline(aes(width = after_stat(density), fill=group), 
                  stat="ydensity", trim=F, alpha = 0.85, scale = 0.25) +
  scale_x_log10() +
  geom_smooth() +
  scale_y_continuous(trans = scales::log10_trans(),breaks = c(1, 10, 100, 1000, 10000), lim = c(10,10000), expand = c(0,0)) +
  scale_fill_discrete(name = "Tech. Q.", labels = c("1-3", "4", "5", "6", "7","8","9","10")) +
  labs(caption = str_c("Comments on Dodd-Frank Rules, N = ", nrow(efficacyXsophistication) %>% prettyNum(big.mark = ",")),
       y = "Lobbying Success\n(Words From Comment\nAdded to Final Rule)",
       x = "Technical Terms in Comment",
       color = "") 
# plot points softly behind
# add regression line 


# 2 Boxplot
efficacyXsophistication %>% 
  dplyr::filter(org == "Matched organization") %>%
  #filter(efficacy > 0) %>%
  arrange(dictionary_terms) %>%
  mutate(
    log_efficacy = log10(efficacy + 1),
    log_dictionary_terms = log10(dictionary_terms + 1),
    log_dictionary_terms_quantiles = ntile(log_dictionary_terms, 10),
    group = as.factor(log_dictionary_terms_quantiles),
    bin=cut_width(log_dictionary_terms, width=1, boundary=0) %>%
      stringr::str_remove("\\(|\\[") %>%
      stringr::str_remove(",.{1,}") %>%
      as.numeric(),
    bin = round(10^bin,-1) %>%
      as.factor() %>%
      prettyNum(big.mark = ",") 
  ) %>% 
  filter(!is.na(group)) %>%
  ggplot(aes(x=bin, y=efficacy)) +
  geom_jitter(aes(x=bin, y=efficacy), alpha = 0.1) +
  geom_boxplot(width=0.65, fill="white") +
  geom_point(aes(x=4,y=10131),colour="red") +
  annotate(y = 10131, x = 3, color = "red", geom = "text", label = "U.S. Chamber of Commerce et al.", vjust = -.8, hjust = -0.1, size = 3.5)+ 
  scale_y_continuous(trans = scales::log10_trans(),breaks = c(1, 10, 100, 1000, 10000), lim = c(10,14000), expand = c(0,0)) +
  labs(caption = str_c("Comments on Dodd-Frank Rules, N = ", nrow(efficacyXsophistication) %>% prettyNum(big.mark = ","),
                       "\n x-axis are bins on the log scale"),
       y = "Lobbying Success\n(Words From Comment\nAdded to Final Rule)",
       x = "Technical Terms in Comment",
       color = "") 
  
  
  annotate(y = log10(10000), x = 3, color = "red", 
           geom = "text", label = "U.S. Chamber of Commerce et al.", 
           vjust = .8, hjust = -0.05, size = 3.5) 
  

### boxplot function 

custom_box <- function(df, filter_full_expression, x_var, y_var){
  # It's a 
  if(missing(filter_full_expression)){
    print("Not Filtering")
  } else {
    df %<>%
      dplyr::filter(org == filter_full_expression)
  }
  
  box_to_return <- df %>% 
    arrange(all_of(x_var)) %>%
    mutate(
      # log_efficacy = log10(efficacy + 1),
      # log_dictionary_terms = log10(dictionary_terms + 1),
      # log_dictionary_terms_quantiles = ntile(log_dictionary_terms, 10),
      group = as.factor(log_dictionary_terms_quantiles),
      bin=cut_width(all_of(x_var), width=1, boundary=0,closed = "right") %>%
        stringr::str_remove("\\(|\\[") %>%
        stringr::str_remove(",.{1,}") %>%
        as.numeric(),
      bin = round(10^bin,-1) %>%
        as.factor() %>%
        prettyNum(big.mark = ",") 
    ) %>% 
    filter(!is.na(group)) %>%
    ggplot(aes(x=bin, y=all_of(y_var))) +
    geom_jitter(aes(x=bin, y=efficacy), alpha = 0.1) +
    geom_boxplot(width=0.75, fill="white") + 
    scale_y_continuous(trans = scales::log10_trans(),breaks = c(1, 10, 100, 1000, 10000), lim = c(10,10000), expand = c(0,0)) 
  return(box_to_return)
}


custom_box(df = efficacyXsophistication,x_var = "dictionary_terms",y_var = "efficacy",
           filter_full_expression = 'org == "Matched organization"'
            )
  labs(caption = str_c("Comments on Dodd-Frank Rules, N = ", nrow(efficacyXsophistication) %>% prettyNum(big.mark = ","),
                       "\n x-axis are bins on the log scale"),
       y = "Lobbying Success\n(Words From Comment\nAdded to Final Rule)",
       x = "Technical Terms in Comment",
       color = "") 

# 2.1 # Violin Plot
ggplot(efficacyXsophistication, aes(x=log(dictionary_terms), y=log(efficacy))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Boxplot of Sophistication and Efficacy",x="Sophistication (Log))", y = "Efficacy (Log)") + 
  theme_classic()

# Blend

# Hexbin
library(hexbin)

hex <- hexbin(y = log(efficacyXsophistication$efficacy + 1), x = log(efficacyXsophistication$dictionary_terms + 1), )
plot(hex, colramp = colorRampPalette(hcl.colors(12)))

efficacyXsophistication %>%
  dplyr::group_by(dictionary_terms) %>% 
  arrange(desc(dictionary_terms)) %>%
  dplyr::add_count(dictionary_terms) %>% View()

https://wilkelab.org/ggridges/reference/geom_vridgeline.html

efficacyXsophistication 

library(ggridges)  



ggplot(efficacyXsophistication, aes(x=log(efficacy+1), y=log(dictionary_terms+1), width =after_stat(density))) +
  geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 10)


ggplot(iris, aes(x=Species, y=Sepal.Width, width = ..density.., fill=Species)) +
  geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 2)


# Data
set.seed(1)
x <- rnorm(5000)
y <- rnorm(5000)

hex <- hexbin(x, y)
plot(hex, colramp = colorRampPalette(hcl.colors(12)))

##############
x1 <- rnorm(1000, 100, 50)
x2 <- x1 + rnorm(1000, 150, 80)
y <- 30 + 20 * x1 + 50 * x2 + rnorm(1000, mean = 0, sd = 40) 
fake_data <- tibble(y, x1, x2)
ggplot(fake_data, aes(x = x1, y = y)) + geom_point()


residualized_binscatter_manual <- function(long_formula, aux_formula, data){
  # long_formula <- "y ~ x1 + x2"
  # aux_formula <- "x1 ~ x2"
  
  long_formula <- as.formula(long_formula)
  aux_formula <- as.formula(aux_formula)
  
  long_reg <- lm(long_formula,data = data)
  aux_reg <- lm(aux_formula,data = data)
  
  short_reg_df <- tibble(
    y = data$y,
    x_tilde = aux_reg$residuals
  )
  
  short_reg <- lm(y ~ x_tilde, data = short_reg_df)
  
  short_reg_coefs <- broom::tidy(short_reg) 
  
  x_bins <- short_reg$model %>% tibble() %>%
    mutate(
      x_bins = ntile(x_tilde, 17)
    ) %>%
    group_by(x_bins) %>%
    mutate(
      x_mean_by_tile = mean(x_tilde),
      y_mean_by_tile = mean(y),
      weight = n(),
    ) 
  
  # this is a residualized binscatter
  return_plot <- ggplot(x_bins, aes(x = x_mean_by_tile, y = y_mean_by_tile, weight = weight)) + 
    geom_point() + 
    geom_smooth(method = "lm")+
    #ggplot2::geom_abline(slope = short_reg_coefs$estimate[2], intercept = short_reg_coefs$estimate[1])
    theme_minimal()
  
  return(return_plot)
}

residualized_binscatter_manual("y~x1+x2", "x1~x2",data=fake_data) 

bivariate_binscatter_manual <- function(long_formula, data, tiles){
  # long_formula <- "Total_Legal_Citations ~ MeanContribAmount"

  long_formula <- as.formula(long_formula)

  long_reg <- lm(long_formula,data = b)

  x_bins <- long_reg$model %>% tibble() %>%
    rename(x = 2, y = 1) %>%
    arrange(y, x) %>%
    mutate(
      x_bins = ntile(x, tiles)
    ) %>%
    group_by(x_bins) %>%
    mutate(
      x_mean_by_tile = round(median(x), 1),
      y_mean_by_tile = round(median(y),1),
      weight = n(),
    ) %>% 
    ungroup() %>% 
    dplyr::select(y_mean_by_tile, x_mean_by_tile, weight) %>% 
    distinct() %>%
    arrange(y_mean_by_tile, x_mean_by_tile)
  
  # this is a residualized binscatter
  return_plot <- ggplot(x_bins, aes(x = x_mean_by_tile, y = y_mean_by_tile, size = weight)) + 
    geom_point() + 
    geom_smooth(method = "lm")+
    #ggplot2::geom_abline(slope = short_reg_coefs$estimate[2], intercept = short_reg_coefs$estimate[1])
    theme_minimal() + theme(legend.position = "none")
  
  return(return_plot)
}

bivariate_binscatter_manual("Total_Legal_Citations ~ MeanContribAmount", tiles = 100, data=b)


binsreg::binsreg(y = y, x = x1, data = data.frame(fake_data))

summary(binsreg::binsregselect(y = y, x = x1, data = data.frame(fake_data)))




x <- runif(500); y <- sin(x)+rnorm(500); x2 <- 3 * runif(500) 
## Binned scatterplot
binsreg::binsreg(y,x, w = x2)


ggplot2::geom_ribbon(data = short_reg, aes(ymin = conf.low, ymax = conf.high), 
                       alpha=0.1, 
                       linetype="dashed",
                       color="grey")


geom_ribbon(data = meetings_2007, aes(ymin = conf.low, ymax = conf.high), 
            alpha=0.1, 
            linetype="dashed",
            color="grey")
short_reg



library(binsreg)

binsreg::binsreg(y = y, x = x1, data = fake_data %>% as.data.frame())

ggplot(fake_data, aes(x = x1, y = y)) + geom_point()
