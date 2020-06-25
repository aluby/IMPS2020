data {
  int<lower=1> I;               // # questions
  int<lower=1> J;               // # persons
  int<lower=1> N1;              // # observations
  int<lower=1> N2;               // # observations
  int<lower=1> N3;               // # observations
  int<lower=1> N4;               // # observations
  int<lower=1> N5;               // # observations
  int<lower=1, upper=I> ii1[N1];  // question for n
  int<lower=1, upper=I> ii2[N2];  // question for n
  int<lower=1, upper=I> ii3[N3];  // question for n
  int<lower=1, upper=I> ii4[N4];  // question for n
  int<lower=1, upper=I> ii5[N5];  // question for n
  int<lower=1, upper=J> jj1[N1];  // person for n
  int<lower=1, upper=J> jj2[N2];  // person for n
  int<lower=1, upper=J> jj3[N3];  // person for n
  int<lower=1, upper=J> jj4[N4];  // person for n
  int<lower=1, upper=J> jj5[N5];  // person for n
  int<lower=0, upper=1> y1[N1];   // decision 1 for n
  int<lower=0, upper=1> y2[N2];   // decision 2 for n
  int<lower=0, upper=1> y3[N3];   // decision 3 for n
  int<lower=0, upper=1> y4[N4];   // decision 4 for n
  int<lower=0, upper=1> y5[N5];   // decision 5 for n
  int<lower=1> K;               // # splits in tree
  matrix[I,2] V;                // item covariates
}
parameters {
  matrix[K,I] b_tilde;
  matrix[K,J] theta_tilde;
  cholesky_factor_corr[K] L_OmegaT;
  vector<lower=0>[K] sigmaT;
  cholesky_factor_corr[K] L_OmegaB;
  vector<lower=0>[K] sigmaB;
  matrix[2, K] beta;
}
transformed parameters {
  matrix[I, K] b;
  matrix[J, K] theta;
  b = (diag_pre_multiply(sigmaB, L_OmegaB) * b_tilde)';
  theta = (diag_pre_multiply(sigmaT, L_OmegaT) * theta_tilde)';
}
model {
  L_OmegaB ~ lkj_corr_cholesky(4);
  sigmaB ~ cauchy(0, 2.5);
  to_vector(b_tilde) ~ normal(to_vector((V*beta)'),1);
  L_OmegaT ~ lkj_corr_cholesky(4);
  sigmaT ~ cauchy(0, 2.5);
  to_vector(theta_tilde) ~ normal(0,1);
  to_vector(beta) ~ normal(0,5);
  y1 ~ bernoulli_logit(theta[jj1,1] - b[ii1,1]);
  y2 ~ bernoulli_logit(theta[jj2,2] - b[ii2,2]);
  y3 ~ bernoulli_logit(theta[jj3,3] - b[ii3,3]);
  y4 ~ bernoulli_logit(theta[jj4,4] - b[ii4,4]);
  y5 ~ bernoulli_logit(theta[jj5,5] - b[ii5,5]);
}
generated quantities {
  corr_matrix[K] OmegaB;
  corr_matrix[K] OmegaT;
  OmegaB = L_OmegaB * L_OmegaB';
  OmegaT = L_OmegaT * L_OmegaT';
}
