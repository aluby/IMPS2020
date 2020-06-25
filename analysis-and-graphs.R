library(tidyverse)
library(bayesplot)
library(blackboxstudyR)

fbi <- TestResponses
fbi$exID = as.integer(as.factor(fbi$Examiner_ID))
fbi$qID = as.integer(as.factor(fbi$Pair_ID))

pres_theme = 
  theme_minimal() + 
  theme(text = element_text(family="Graphik", face = "bold", size = 10),
        strip.text = element_text(face = "bold", size = 10))

fbi %>%
  select(exID, Compare_Value, Latent_Value) %>%
  group_by(exID) %>%
  summarize(., n_nv = sum(Latent_Value == 'NV'), 
            n_inc = sum(Compare_Value == 'Inconclusive', na.rm = TRUE)) %>%
  select(exID, n_nv, n_inc) %>%
  gather(., key = "resp", value = "obs", -exID) %>%
  mutate(., resp2 = ifelse(resp == 'n_nv', 'No Value', 'Inconclusive')) %>%
  ggplot(., aes(x = obs, fill = resp2)) +
  geom_histogram(bins = 25) +
  facet_grid(cols = vars(resp2)) +
  scale_fill_viridis_d(end = .5) +
  pres_theme +
  theme(legend.position = "none") +
  labs(
    x = 'Number Reported',
    y = 'N Examiners') -> inc_nv_hist

fbi %>%
  select(exID, Inconclusive_Reason, Exclusion_Reason) %>%
  group_by(exID) %>%
  summarize(., n_close = sum(Inconclusive_Reason == "Close", na.rm = TRUE),
            n_ins = sum(Inconclusive_Reason == "Insufficient", na.rm = TRUE),
            n_no = sum(Inconclusive_Reason == "Overlap", na.rm = TRUE),
            n_min = sum(Exclusion_Reason == "Minutiae", na.rm = TRUE),
            n_pattern = sum(Exclusion_Reason == "Pattern", na.rm = TRUE)) %>%
  select(exID, n_close, n_ins, n_no) %>%
  pivot_longer(., -exID, names_to = "resp", values_to = "count") %>%
  mutate(., 
         resp2 = case_when(
           resp == "n_close" ~ "Close",
           resp == "n_ins" ~ "Insufficient Info",
           resp == "n_no" ~ "No Overlap"
         )) %>%
  ggplot(., aes(x = count, fill = resp)) +
  geom_histogram(bins = 25) +
  facet_grid(cols = vars(resp2)) +
  scale_fill_viridis_d(end = .5) +
  pres_theme +
  theme(legend.position = "none") +
  labs(
    x = 'Number Reported',
    y = 'N Examiners') -> inc_reason_hist

fbi %>%
  dplyr::filter(.,Pair_ID  %in% c("M003064", "N057413") )%>%
  mutate(
    Latent = ifelse(Latent_Value == "NV", "NV", "Has Value"),
    source_long = fct_explicit_na(case_when(
      Compare_Value == "Exclusion" ~ 'Exclusion', 
      Compare_Value == "Individualization" ~ 'Individ.',
      Compare_Value == "Inconclusive" ~ 'Inconclusive'
    ), na_level = "No Value"),
    Source = fct_explicit_na(case_when(
      Compare_Value == "Exclusion" ~ 'Exc', 
      Compare_Value == "Individualization" ~ 'ID',
      Compare_Value == "Inconclusive" ~ 'Inc'
    ), na_level = "NV"),
    Reason = factor(case_when(
      Inconclusive_Reason == "Close" ~ "Close",
      Inconclusive_Reason == "Insufficient" ~ "Ins",
      Inconclusive_Reason == "Overlap" ~ "No Ov.",
      Exclusion_Reason == "Pattern" ~ "`Pat",
      Exclusion_Reason == "Minutiae" ~ "`Min",
      Compare_Value == "Individualization" ~ "ID",
      Latent_Value == "NV" ~ "NV"
    ), levels = c("`Min", "`Pat", "ID", "Close", "Ins", "No Ov.", "NV")),
    Difficulty = fct_explicit_na(substr(Difficulty, 1, 1), na_level = "NV"),
    Pair_ID = paste("Item:", Pair_ID)
  ) %>%
  group_by(Latent, Source, Reason, Pair_ID, source_long) %>%
  tally() %>%
  ggforce::gather_set_data(., 1:3) %>%
  mutate(
    x = factor(x, levels = c("Latent", "Source", "Reason"))
  ) %>%
  ggplot(., aes(x, id = id, split = y, value = n)) +
  ggforce::geom_parallel_sets(aes(fill = source_long), alpha = .7, axis.width = 0.05, sep = .1) + 
  ggforce::geom_parallel_sets_axes(axis.width = 0.1, sep = .1) + 
  ggforce::geom_parallel_sets_labels(colour = 'white', size = 3, sep = .1) +
  facet_grid(cols = vars(Pair_ID)) + 
  scale_fill_viridis_d("Source\nDecision", end = .75) +
  labs(x = '') +
  pres_theme +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 10)) +
  labs(x = "Decision",
       y = "N Respondents") -> pres_flow


## Load IRTree + proficiency data (created from modeling script)

irtree_v3 = readRDS("stan-output/stan-irtree-v3.rds")
stan_rasch = readRDS("stan-output/rasch.rds")

theta_summary = rstan::summary(irtree_v3, pars = c("theta"), probs = c(.025, .975))$summary
b_summary = rstan::summary(irtree_v3, pars = c("b"), probs = c(.025, .975))$summary

rasch_summary = rstan::summary(stan_rasch, pars = c("theta"), probs = c())$summary

as_tibble(rasch_summary) %>%
  mutate(param = rownames(rasch_summary),
         proficiency = mean) %>%
  separate(., param, into = c("theta", "exID"), remove = FALSE, convert = TRUE) %>%
  select(proficiency, exID) -> rasch_prof

fbi %>%
  group_by(exID) %>%
  summarize(
    nv_rate = mean(Latent_Value == "NV", na.rm = T),
    inc_rate = mean(Compare_Value == "Inconclusive", na.rm = T),
    ind_rate = mean(Compare_Value == "Individualization", na.rm = T),
    exc_rate = mean(Compare_Value == "Exclusion", na.rm = T)
  ) %>%
  left_join(rasch_prof, by = "exID") -> ex_summary

as_tibble(theta_summary) %>%
  mutate(param = rownames(theta_summary)) %>%
  separate(., param, into = c("theta", "exID", "split"), remove = FALSE, convert = TRUE) %>% 
  left_join(., ex_summary, by = "exID") %>%
  mutate(
    split_lab = factor(split, levels = c(1, 2, 3, 4, 5), ordered = TRUE, labels = expression(paste(theta[1],"  (No Value)"),
                                                                                             paste(theta[1],"  (Pattern Excl)"),
                                                                                             paste(theta[1],"  (Match)"),
                                                                                             paste(theta[1],"  (Individ.)"),
                                                                                             paste(theta[1],"  (Minutiae Excl)")))
  ) %>%
  ggplot(., aes(x = proficiency, y = mean, ymin = `2.5%`, ymax = `97.5%`, col = split)) + 
  geom_pointrange(size = .5, fatten = 1, alpha = .8) + 
  facet_grid(cols = vars(split_lab), labeller = label_parsed) +
  pres_theme +
  theme(legend.position = "none",
        strip.text = element_blank()) + 
  scale_color_viridis_c(end = .75) +
  labs(
    x = "Proficiency (from scored model)",
    y = "IRTree Estimate"
  ) -> theta_scatter

examiner_points = as_tibble(theta_summary) %>%
  mutate(param = rownames(theta_summary)) %>%
  separate(., param, into = c("theta", "exID", "split"), remove = FALSE, convert = TRUE) %>%
  select(mean, se_mean, exID, split)

item_points = as_tibble(b_summary) %>%
  mutate(param = rownames(b_summary)) %>%
  separate(., param, into = c("b", "qID", "split"), remove = FALSE, convert = TRUE) %>%
  select(mean, se_mean, qID, split)

logistic = function(x) return(1/(1+exp(-x)))

## Create answer key based on IRTree parameters

ans_key = item_points %>%
  mutate(theta = 0) %>%
  pivot_wider(., id_cols = c(qID, theta), names_prefix = "b", names_from = split, values_from = mean) %>%
  mutate(
    p_nv = logistic(theta - b1),
    p_ex_pat = (1-logistic(theta - b1)) * (logistic(theta - b2)),
    p_id = (1-logistic(theta - b1)) * (1 - logistic(theta - b2)) * (logistic(theta - b3)) * (logistic(theta - b4)),
    p_close = (1-logistic(theta - b1)) * (1 - logistic(theta - b2)) * (logistic(theta - b3)) * (1 - logistic(theta - b4)),
    p_ex_min = (1-logistic(theta - b1)) * (1 - logistic(theta - b2)) * (1 - logistic(theta - b3)) * (logistic(theta - b5)),
    p_inc = (1-logistic(theta - b1)) * (1 - logistic(theta - b2)) * (1 - logistic(theta - b3)) * (1 - logistic(theta - b5))
  ) %>%
  mutate(
    ans = apply(.[,8:13], 1, function(x) names(x)[which.max(x)])
  )


## CHECK: sum = 1?

table(ans_key$p_nv + ans_key$p_ex_pat + ans_key$p_id + ans_key$p_close + ans_key$p_ex_min + ans_key$p_inc) 

## Example q: 556

ex55 = examiner_points %>%
  filter(exID == 55) %>%
  pivot_wider(., id_cols = c(exID), names_prefix = "theta", names_from = split, values_from = mean) 

ans_key %>%
  filter(qID == 556) %>%
  mutate(
    p_nv = logistic(ex55$theta1 - b1),
    p_ex_pat = (1-logistic(ex55$theta1 - b1)) * (logistic(ex55$theta2 - b2)),
    p_id = (1-logistic(ex55$theta1 - b1)) * (1 - logistic(ex55$theta2 - b2)) * (logistic(ex55$theta3 - b3)) * (logistic(ex55$theta4 - b4)),
    p_close = (1-logistic(ex55$theta1 - b1)) * (1 - logistic(ex55$theta2 - b2)) * (logistic(ex55$theta3 - b3)) * (1 - logistic(ex55$theta4 - b4)),
    p_ex_min = (1-logistic(ex55$theta1 - b1)) * (1 - logistic(ex55$theta2 - b2)) * (1 - logistic(ex55$theta3 - b3)) * (logistic(ex55$theta5 - b5)),
    p_inc = (1-logistic(ex55$theta1 - b1)) * (1 - logistic(ex55$theta2 - b2)) * (1 - logistic(ex55$theta3 - b3)) * (1 - logistic(ex55$theta5 - b5))
  )

## Summary of answer key

table(ans_key$ans)

## Save figures

ggsave("figures/inc-nv-hists.png", inc_nv_hist, width = 5, height = 2, units = "in")
ggsave("figures/inc-reason-hists.png", inc_reason_hist, width = 6, height = 2, units = "in")
ggsave("figures/decision-flow.png", pres_flow, width = 6, height = 3, units = "in")
ggsave("figures/theta-scatter.png", theta_scatter, width = 7, height = 3, units = "in")



