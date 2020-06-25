library(rstan)
library(blackboxstudyR)
library(dplyr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fbi <- TestResponses %>% mutate(
  exID = as.integer(as.factor(fbi$Examiner_ID)),
  qID = as.integer(as.factor(fbi$Pair_ID)),
  response = case_when(
    Latent_Value == 'NV' ~ 0,
    Compare_Value == 'Individualization' ~ 1,
    Inconclusive_Reason  == 'Close' ~ 2,
    Inconclusive_Reason == 'Insufficient' ~ 3,
    Inconclusive_Reason == 'Overlap' ~ 4,
    Exclusion_Reason == 'Minutiae' ~ 5,
    Exclusion_Reason == 'Pattern' ~ 6)
)


item_lookup = fbi %>%
  select(qID, Mating) %>%
  unique() %>%
  arrange(qID)

## IRTree V3
irtree_v3 = fbi %>% 
  select(exID, qID, response) %>%
  mutate(
    node1 = ifelse(response == 0, 1, 0),
    node2 = case_when(
      response == 6 ~ 1,
      response %in% c(1, 2, 3, 4, 5) ~ 0),
    node3 = case_when(
      response %in% c(3, 4, 5) ~ 0,
      response %in% c(1, 2) ~ 1),
    node4 = case_when(
      response == 1 ~ 1,
      response == 2 ~ 0),
    node5 = case_when(
      response == 5 ~ 1,
      response %in% c(3, 4) ~ 0)
  )

# Check for errors: 
irtree_v3 %>% select(response, node1, node2, node3, node4, node5) %>% unique()

## Step 2: Create a list formatted for stan (stan doesn't like NA's)

stan_list_v3 = list(
  y1 = irtree_v3$node1,
  ii1 = irtree_v3$qID,
  jj1 = irtree_v3$exID,
  y2 = irtree_v3$node2[!(is.na(irtree_v3$node2))],
  ii2 = irtree_v3$qID[!is.na(irtree_v3$node2)],
  jj2 = irtree_v3$exID[!is.na(irtree_v3$node2)],
  y3 = irtree_v3$node3[!(is.na(irtree_v3$node3))],
  ii3 = irtree_v3$qID[!is.na(irtree_v3$node3)],
  jj3 = irtree_v3$exID[!is.na(irtree_v3$node3)],
  y4 = irtree_v3$node4[!(is.na(irtree_v3$node4))],
  ii4 = irtree_v3$qID[!is.na(irtree_v3$node4)],
  jj4 = irtree_v3$exID[!is.na(irtree_v3$node4)],
  y5 = irtree_v3$node5[!(is.na(irtree_v3$node5))],
  ii5 = irtree_v3$qID[!is.na(irtree_v3$node5)],
  jj5 = irtree_v3$exID[!is.na(irtree_v3$node5)],
  I = length(unique(irtree_v3$qID)),
  J = length(unique(irtree_v3$exID)),
  N1 = nrow(irtree_v3),
  N2 = sum(!is.na(irtree_v3$node2)),
  N3 = sum(!is.na(irtree_v3$node3)),
  N4 = sum(!is.na(irtree_v3$node4)),
  N5 = sum(!is.na(irtree_v3$node5)),
  V = cbind(rep(1, length(unique(fbi$qID))), item_lookup$Mating=='Mates'),
  K = 5
)

stan_irtree_v3 <- stan("stan-files/hypothesized-binary-irtree.stan", data = stan_list_v3,
                       iter = 10000, chains = 4)
saveRDS(stan_irtree_v3, file = "stan-output/stan-irtree-v3.rds")


## We'll also fit a basic IRT model

fbi %>%
  mutate(
    correct = ifelse((fbi$Compare_Value == "Individualization" & fbi$Mating == "Mates") | (fbi$Compare_Value == "Exclusion" & fbi$Mating == "Non-mates"),
1, 0)
  ) -> fbi

stan_rasch_list = list(
  y = fbi$correct[!is.na(fbi$correct)],
  ii = fbi$qID[!is.na(fbi$correct)],
  jj = fbi$exID[!is.na(fbi$correct)],
  I = length(unique(fbi$qID)),
  J = length(unique(fbi$exID)),
  N = length(fbi$correct[!is.na(fbi$correct)])
)

stan_rasch <- stan("stan-files/rasch.stan", data = stan_rasch_list,
                       iter = 4000, chains = 4)

saveRDS(stan_rasch, file = "stan-output/rasch.rds")
