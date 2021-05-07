# ------------Plotting individual CSV  ------# 
# exploring plant stem diameter and height relationship 

# @knitr analysis-setup
library(dplyr)
library(ggplot2)

# load in dataset & select 3 relevant columns 
individual <- readr::read_csv(here::here("data", "individual.csv")) %>%
  select(stem_diameter, height, growth_form)

summary(individual)
# @knitr analysis-setupextra
individual %>%
  ggplot(aes(x = log(stem_diameter))) +
  geom_density(fill = "grey", colour = "blue")

individual %>%
  ggplot(aes(x = log(height))) + 
  geom_density(fill = "grey", colour = "yellow")

# @knitr analysis-filter-data 
# remove liana and NA values  
analysis_df <- individual %>%
  filter(!is.na(growth_form), growth_form != "liana")

# @knitr analysis
analysis_df %>%
  ggplot(aes(x = growth_form, colour = growth_form, 
             fill = growth_form)) + geom_bar()


## organise bars on a bar chart in ascending order 


# @knitr analysis-set-factor-levels
gf_levels <- table(analysis_df$growth_form) %>%
  sort()%>%
  names()
analysis_df <- analysis_df %>%
  mutate(growth_form = factor(growth_form,
                              levels = gf_levels))
## @knitr analysis-fig1-barplot
# change the bar plot to run sideways, and make bars transparent
# and remove legend
analysis_df %>%
  ggplot(aes(y = growth_form, colour = growth_form, 
             fill = growth_form)) + geom_bar(alpha = 0.5, show.legend = FALSE)


##Plotting multiple distributions (of a continuous variable) in a single plot##
# @knitr violinplot
analysis_df %>%
  ggplot(aes(x = log(stem_diameter), group = growth_form, 
             colour = growth_form, fill = growth_form))+
  geom_density(alpha = 0.5, trim = TRUE) +
  facet_wrap(~growth_form)

###### Using a violin plot instead of a density plot 
## @knitr analysis-fig2-violinplots 
analysis_df %>%
  ggplot(aes(x = log(stem_diameter),y = growth_form, 
             colour = growth_form, fill = growth_form))+
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE, colour = "black")

## changing data to be two long data columns 
## put height and width info side by side 
analysis_df %>% 
  tidyr::pivot_longer(cols = c(stem_diameter, height),
                     names_to ="var", 
                     values_to = "value") %>%
  ggplot(aes(x = log(value),y = growth_form, 
             colour = growth_form, fill = growth_form))+
  geom_violin(alpha = 0.5, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

## @knitr analysis-lm-overall
lm_growth <- lm(log(stem_diameter)~ log(height) * growth_form,
                analysis_df)

lm_growth %>%
  broom::glance()
lm_growth %>%
  broom::tidy()


## @knitr analysis-lm-fig3-overall 
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

## @knitr analysis-lm-growth
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form, analysis_df) 
lm_growth %>%
  broom::glance()
lm_growth %>%
  broom::tidy()

# @knitr analysis-lm-fig4-growth
analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter),
             colour = growth_form)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm")






