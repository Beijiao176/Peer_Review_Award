library(fect)
library(tidyverse)
library(readxl)
library(panelView)
library(plm)
library(patchwork)
library(MatchIt)
library(eoffice)
library(rstatix)
library(slider)
library(lmtest)


# Read the data.
# setwd('C:\\....')
data = read_csv('data\\data.csv')


# Matching
## matching based on the first period (2012).
data_2012 = data %>% 
  filter(year==2012) %>% 
  filter(award_year==2018 | award_year==9999) %>% 
  filter(r>=1)

m_out = data_2012 %>% 
  matchit(is_winner ~ age+p_cumu+c_cumu+discipline+age_p+gender+age_c,
          data = .,
          method = "nearest", distance = "glm", replace=TRUE)

m_data_2012 = 
  data_2012 %>% 
  match.data(m_out, data = .,
             distance = "prop.score")

data_match = data %>% 
  filter(id %in% as_vector(m_data_2012['id']))


## Balance test: plot the SMD before and after matching.
summary_m_out = summary(m_out)

before_smd = as_tibble(summary_m_out$sum.all, rownames = "var") %>% 
  select(var, `Std. Mean Diff.`) %>% mutate(type='before')

after_smd = as_tibble(summary_m_out$sum.matched, rownames = "var") %>% 
  select(var, `Std. Mean Diff.`) %>% mutate(type='after')

smd_matched = bind_rows(before_smd, after_smd)

levels_without_distance <- setdiff(smd_matched$var, "distance")
sorted_levels <- c("distance", sort(levels_without_distance))

p_smd = smd_matched %>% 
  mutate(var = factor(var, levels=sorted_levels)) %>%
  mutate(`Std. Mean Diff.` = abs(`Std. Mean Diff.`)) %>%
  ggplot() +
  geom_point(aes(y=var, x=`Std. Mean Diff.`, color=type)) + 
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=2) +
  scale_x_continuous(breaks = c(0, 0.05, 0.1, 0.2, 0.3)) + 
  theme_bw()


# DID
f_fect = function(data, formula, force) {
  out.ife = 
    data %>% 
    fect(formula,
         data = .,
         index = c("id","year"), 
         force = force,
         method = 'ife',
         nboots = 200, 
         se = TRUE,
         parallel = TRUE)
  return(out.ife)
}


set.seed(1234)
formula_control_yes = r ~ did + age_ln + l_p + noa_b3 + c_cumu + age_p + age_c + noc
formula_control_no = r ~ did

model_ife_control_no_fix_yes = f_fect(data_match, formula_control_no, force='two-way')
model_ife_control_yes_fix_yes = f_fect(data_match, formula_control_yes, force='two-way')

model_ife_control_no_fix_yes
## plot the atts
p_atts = plot(model_ife_control_yes_fix_yes, main = "Estimated ATTs") +
  theme(panel.grid = element_blank())
p_atts

## parallel trends testing
p_pre_trend = plot(model_ife_control_yes_fix_yes, type="equiv", ylim=c(-8,8),
                   cex.legend=0.6, 
                   main="Testing Pre-Trend",
                   cex.text = 0.8) +
  theme(panel.grid = element_blank())


## placebo testing
out_placebo <- fect(formula_control_yes, data=data_match, index = c("aid", "year"),
                    force = "two-way", method = "ife",  r=2, CV = 0,
                    parallel = TRUE, se=TRUE, min.T0=3, 
                    placeboTest = TRUE, placebo.period = c(-1, 0))

p_placebo = plot(out_placebo, cex.text = 0.8, 
                 stats = c("placebo.p","equiv.p"), 
                 main = "Placebo tests", ylim=c(-10,2))  +
  theme(panel.grid = element_blank())
