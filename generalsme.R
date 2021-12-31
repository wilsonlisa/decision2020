setwd("~/Documents/Decision2020/decision2020/")

library(sme)
# library(strapgod)
library(reshape2)
library(ggformula)
library(ggrepel)
library(pander)
library(tidyverse)
library(foreach)
library(doParallel)
library(parallel)

to_spread <- function(x) {
    100*(exp(x)-1)/(exp(x+1))
}

# data
url <- "https://projects.fivethirtyeight.com/polls-page/"
poll_file <- "president_polls.csv"
download.file(paste0(url, poll_file), poll_file, 
              mode = "wb")
# download.file(paste0(url, poll_file), paste(poll_file, Sys.Date(), sep="_"),
# mode = "wb")

polls <- read_csv(poll_file,
                  col_types = cols(.default = col_character(), 
                                   question_id = col_double(),
                                   poll_id = col_double(),
                                   cycle = col_double(),
                                   pollster_id = col_double(),
                                   sponsor_ids = col_number(),
                                   pollster_rating_id = col_double(),
                                   sample_size = col_double(),
                                   internal = col_logical(),
                                   tracking = col_logical(),
                                   nationwide_batch = col_logical(),
                                   # candidate_id = col_double(),
                                   pct = col_double()))

# a: all adults; lv: likely voters; rv: registered voters; v: voters

# data cleaning
polls$start_date <- as.Date(polls$start_date, "%m/%d/%y")
polls$end_date <- as.Date(polls$end_date, "%m/%d/%y")
polls$fte_grade <- as.factor(polls$fte_grade)
polls$methodology <- as.factor(polls$methodology)
polls$population <- as.factor(polls$population)
polls$population_full <- as.factor(polls$population_full)
polls$stage <- as.factor(polls$stage)
polls$candidate_party <- as.factor(polls$candidate_party)

polls <- polls %>%
    mutate(mid_date = end_date - (1 + as.numeric(end_date - start_date)) %/% 2,
           state = as.factor(replace_na(state, "Nationwide")))

polls_apr <- polls %>%
    filter(mid_date >= "2020-04-01",
           state != "Nationwide",
           candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump"))

polls_sept <- polls %>%
    filter(mid_date >= "2020-09-01",
           state != "Nationwide",
           candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump"))

# create ratio variable
q_id <- polls_sept %>%
    dplyr::select(question_id, start_date, end_date) %>%
    distinct() %>%
    pull(question_id)

biden <- rep(NA, length(q_id))
trump <- rep(NA, length(q_id))
ratio <- rep(NA, length(q_id))
spred <- rep(NA, length(q_id))
for (i in 1:length(q_id)){
    temp <- as.data.frame(filter(polls_apr, question_id == q_id[i]))
    if (any(!c("Biden", "Trump") %in% temp$answer)){
        next
    } 
    else{
        biden[i] <- temp[temp$answer == "Biden", "pct"]
        trump[i] <- temp[temp$answer == "Trump", "pct"]
        ratio[i] <- biden[i]/trump[i]
        spred[i] <- biden[i] - trump[i]
    }
}

tr_vals <- data.frame("biden" = biden, "trump" = trump, "ratio" = ratio, "spred" = spred)

polls_re <- polls_sept %>%
    dplyr::select(question_id, poll_id, start_date, end_date, pollster_id, pollster, state) %>%
    distinct() %>%
    bind_cols(tr_vals) %>%
    drop_na()

election_day <- as.Date("2020-11-03")
polls_re <- polls_re %>%
    mutate(days_til_elec = as.numeric(end_date - election_day),
           logratio = log(ratio),
           mid_date = end_date - (1 + as.numeric(end_date - start_date)) %/% 2,
           days_til_elec_mid = as.numeric(mid_date - election_day))

## parameter tuning
# print(head(polls_re, 41), n = Inf, width = Inf)

# round(nrow(polls_re)*.1)

# polls_re %>%
    # arrange(desc(days_til_elec_mid)) %>%
    # print(n = 33, width = Inf)

# polls_re %>%
    # filter(days_til_elec_mid >= -105)

cutoff <- sort(polls_re$days_til_elec_mid, decreasing = TRUE)[round(nrow(polls_re)*.1)]

## Train and test sets
polls_re_tr <- polls_re %>%
    filter(days_til_elec_mid < cutoff) # prev: 141

polls_re_ts <- polls_re %>%
    filter(days_til_elec_mid >= cutoff)

## Lambda choices
lam_mu <- c(0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)
lam_nu <- c(0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)

sme_param <- matrix(list(), nrow = length(lam_mu), ncol = length(lam_nu))
#### this is slow
for (i in 1:length(lam_mu)){
    for (j in 1:length(lam_nu)){
        sme_param[[i, j]] <- sme(object = polls_re_tr$logratio, tme = polls_re_tr$days_til_elec_mid, ind = polls_re_tr$state, lambda.mu = lam_mu[i], lambda.v = lam_nu[j])
    }
}

lam_mu_2 <- c(rep(0.5, 9), rep(1, 9),rep(5, 9), rep(10, 9), rep(50, 9), rep(100, 9), rep(500, 9), rep(1000, 9), rep(5000, 9))
lam_nu_2 <- rep(lam_nu, 9)
## write above loop in parallel

# how to fix for there being multiple out dates (9)
# need to have columns for each date for each state --> maybe not
state_ts <- sort(unique(filter(polls_re_ts, state %in% unique(polls_re_tr$state))$state))

# polls_re_ts %>%
    # filter(state %in% unique(polls_re_tr$state)) %>%
    # print(n = Inf, width = Inf)

tme_ts <- length(unique(polls_re_ts$days_til_elec_mid))
theta_0_param <- matrix(nrow = length(lam_mu)^2, ncol = length(state_ts)*length(unique(polls_re_ts$days_til_elec_mid)))
for (i in 1:length(lam_mu)){
    for (j in 1:length(lam_nu)){
        for (k in 1:length(state_ts)){
            theta_0_param[9*(i-1)+j, (tme_ts*(k-1)+1):(tme_ts*(k-1)+tme_ts)] <- spline(sort(unique(sme_param[[i,j]]$data$tme)), sme_param[[i,j]]$coefficients["mu",] + sme_param[[i,j]]$coefficients[paste("v", state_ts[k], sep=""),], xout = unique(polls_re_ts$days_til_elec_mid), method = "natural")$y
        }
        
    }
}

# woo!
# write.csv(theta_0_param, "theta_0_param.csv", row.names = FALSE)

# just for state & date combos in test set
logratio_ts <- polls_re_ts %>%
    filter(state %in% unique(polls_re_tr$state)) %>%
    group_by(state, days_til_elec_mid) %>%
    summarize(logratio_avg = mean(logratio))

theta_0_param <- matrix(nrow = length(lam_mu)^2, ncol = nrow(logratio_ts))
for (i in 1:length(lam_mu)){
    for (j in 1:length(lam_nu)){
        for (k in 1:nrow(logratio_ts)){
            theta_0_param[9*(i-1)+j, k] <- spline(sort(unique(sme_param[[i,j]]$data$tme)), sme_param[[i,j]]$coefficients["mu",] + sme_param[[i,j]]$coefficients[paste("v", logratio_ts$state[k], sep=""),], xout = logratio_ts$days_til_elec_mid[k], method = "natural")$y
        }
    }
}

write.csv(theta_0_param, "theta_0_param.csv", row.names = FALSE)

mse_param <- rowMeans(sweep(theta_0_param, 2, logratio_ts$logratio_avg)^2)

mse_param_mat <- matrix(mse_param, nrow = 9, ncol = 9) # col are lam_mu, row are lam_nu
write.csv(mse_param_mat, "mse_param_mat.csv", row.names = FALSE)

mse_param_mat <- read.csv("mse_param_mat.csv")

rmse_param_mat <- as.matrix(sqrt(mse_param_mat))

# sort(rmse_param_mat)[1:10]

rmse_param_min <- sort(rmse_param_mat)[1]

rmse_param_ind <- which(as.matrix(rmse_param_mat) == rmse_param_min) # 52; 79; 63
lam_mu_use <- lam_mu[ifelse(rmse_param_ind %% 9 == 0, rmse_param_ind %/% 9, rmse_param_ind %/% 9 + 1)]
lam_nu_use <- lam_nu[ifelse(rmse_param_ind %% 9 == 0, 9, rmse_param_ind %% 9)]

# lam_mu[9]
# lam_nu[7]
# lam_mu = 100, lam_nu = 500
# lam_mu = 5000, lam_nu = 500
# 5000, 5000; again 9/4, 10/30
lam_mu_use <- 5000
lam_nu_use <- 5000

### Refit model with lam_mu, lam_nu as above (and using mid-date)
sme_test <- sme(object = polls_re_tr$logratio, tme = polls_re_tr$days_til_elec_mid, ind = polls_re_tr$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
# plot(sme_test, showConfidenceBands = TRUE)
theta_0_test <- rep(0, nrow(logratio_ts))
for (k in 1:nrow(logratio_ts)){
    theta_0_test[k] <- spline(sort(unique(sme_test$data$tme)), sme_test$coefficients["mu",] + sme_test$coefficients[paste("v", logratio_ts$state[k], sep=""),], xout=logratio_ts$days_til_elec_mid[k], method = "natural")$y
}

# sqrt(mean((theta_0_test - logratio_ts$logratio_avg)^2)) # woo, did it right

### Get forecasts for today
state_gen <- sort(unique(polls_re$state))
state_gen <- state_gen[state_gen != "Nationwide"]
today_num <- as.numeric(Sys.Date() - election_day)

sme_1103 <- sme(object = polls_re$logratio, tme = polls_re$days_til_elec_mid, ind = polls_re$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
plot(sme_1103, showConfidenceBands = TRUE)
theta_0_1103 <- rep(0, length(state_gen))
for (k in 1:length(state_gen)){
    theta_0_1103[k] <- spline(sort(unique(sme_1103$data$tme)), sme_1103$coefficients["mu",] + sme_1103$coefficients[paste("v", state_gen[k], sep=""),], xout=today_num, method = "natural")$y
}

theta_0_1103
spread_0_1103 <- to_spread(theta_0_1103)

pred_1103 <- data.frame(state = sort(unique(polls_re$state)), spread = spread_0_1103)

# sme_0713 <- sme(object = polls_re$logratio, tme = polls_re$days_til_elec_mid, ind = polls_re$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
# plot(sme_0713, showConfidenceBands = TRUE)
# theta_0_0713 <- rep(0, length(state_gen))
# for (k in 1:length(state_gen)){
    # theta_0_0713[k] <- spline(sort(unique(sme_0713$data$tme)), sme_0713$coefficients["mu",] + sme_0713$coefficients[paste("v", state_gen[k], sep=""),], xout=today_num, method = "natural")$y
# }

# theta_0_0713
# spread_0_0713 <- to_spread(theta_0_0713)

### Get bootstrapped covariance matrix
set.seed(42)

boot_full <- tibble()
g <- rep(0, length(state_gen))
s_m <- rep(0, length(state_gen))

for (i in 1:length(state_gen)){
    boot <- polls_re %>%
        filter(state == state_gen[i]) %>%
        group_by(pollster_id, state) %>%
        summarize(m_k = mean(logratio))
    
    # boot_AL %>%
    # print(n=Inf, width=Inf)
    
    g[i] <- mean(boot$m_k)
    s_m[i] <- ifelse(is.na(var(boot$m_k)), 0, var(boot$m_k)) # this is weird, why is the var of a constant not just 0
    eta_k <- data.frame(pollster_id = unique(boot$pollster_id), eta_k = rnorm(length(unique(boot$pollster_id)), 0, sqrt(s_m[i])))
    
    boot <- inner_join(inner_join(polls_re, boot), eta_k) %>%
        mutate(y_prime = logratio - m_k - g[i] + eta_k) 
    
    boot_full <- rbind(boot_full, boot)
}

# print(boot_full, n=Inf, width=Inf)

boot_full$state <- droplevels(boot_full$state)

boot_full_g <- boot_full %>%
    group_by(state, pollster_id)

# get number of unique polls for each pollster in each state
state_n <- boot_full_g %>%
    summarize(n = n()) %>%
    pull(n)

# what an absolute champ of a function
# IT BROKE!
# boot_full_resamp <- samplify(boot_full, times=500, size = state_n, replace=TRUE, key=".samps") %>%
    # collect()

# boot_full %>%
    # sample_n(., size = state_n, replace = TRUE) %>%
    # print(n=Inf, width = Inf)

# https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
# figure out why this works
# faster!
#### but still slow
boot_full_resamp <- list()
for (i in 1:100){
    boot_full_resamp[[i]] <- boot_full_g %>% 
        nest() %>%            
        ungroup() %>% 
        mutate(n = state_n) %>% 
        mutate(samp = map2(data, n, sample_n, replace = TRUE)) %>% 
        select(-data) %>%
        unnest(samp)
}

# awkward but works
# think this takes longer
#### this is SLOW
sme_boot_list <- list()
theta_cur_boot <- matrix(nrow = 100, ncol = length(state_gen))

# figure this out
numCores <- detectCores()
registerDoParallel(numCores)
sme_boot_list <- foreach (i=1:500) %dopar% {
    sme(object = boot_full_resamp[[i]]$y_prime, tme = boot_full_resamp[[i]]$days_til_elec_mid, ind = boot_full_resamp[[i]]$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
}

stopImplicitCluster()

for (i in 1:100){
    #boot_use <- filter(boot_full_resamp, .samps == i)
    sme_boot_list[[i]] <- sme(object = boot_full_resamp[[i]]$y_prime, tme = boot_full_resamp[[i]]$days_til_elec_mid, ind = boot_full_resamp[[i]]$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
}
# try just saving coefficients? tme can be same?
# 4:57 - 5:09 (100, just coef); 5:17 - 6:19 (500)
# 12:34 - 1:30 (7/31); 2:45 - 2:58 (100)
# 4:53 - 5:26 (7/13)

for (i in 1:100){
    for (j in 1:length(state_gen)){
        theta_cur_boot[i,j] <- spline(sort(unique(sme_boot_list[[i]]$data$tme)), sme_boot_list[[i]]$coefficients["mu",] + sme_boot_list[[i]]$coefficients[paste("v", state_gen[j], sep=""),], xout=today_num, method = "natural")$y
    }
}
# 3:17 - 3:55*stopped because ridic (100, combining loops -- way bad)

write.csv(theta_cur_boot, "theta_cur_boot.csv", row.names = FALSE)
# theta_cur_boot <- read.csv("theta_cur_boot.csv")

boot_cov <- cov(theta_cur_boot)
boot_SE <- sqrt(diag(boot_cov))
boot_SE_2 <- boot_SE
boot_SE_2[52] <- 0.1

# boot_SE_t <- sqrt(diag(cov(theta_0_boot)))

## CIs
theta_0_cur_lb <- rep(0, length(state_gen))
theta_0_cur_ub <- rep(0, length(state_gen))
for (i in 1:length(state_gen)){
    theta_0_cur_lb[i] <- theta_0_1103[i] - 1.96*boot_SE[i]
    theta_0_cur_ub[i] <- theta_0_1103[i] + 1.96*boot_SE[i]
}

theta_0_cur_lb
theta_0_cur_ub

## Lil plot --> update
theta_cur_gen_df <- data.frame(est = theta_0_1103, 
                               # res = theta_0_res, 
                              lb = theta_0_cur_lb, ub = theta_0_cur_ub,
                              state = state_gen)

write.csv(theta_cur_gen_df, file = "theta_cur_gen.csv", row.names = FALSE)

theta_cur_gen_df$candidate <- ifelse(theta_cur_gen_df$est > 0, "Biden", "Trump")
theta_cur_gen_df$tossup <- ifelse(theta_cur_gen_df$lb < 0 & theta_cur_gen_df$ub > 0, 1, 0)

write.csv(theta_cur_gen_df, "final_pred.csv", row.names = FALSE)

# theta_coverage <- mean(theta_ST_mid_df$res < theta_ST_mid_df$ub & theta_ST_mid_df$res > theta_ST_mid_df$lb)

### Electoral votes
ecstates <- read.csv("electoralstate.csv")

theta_cur_ec <- left_join(ecstates, theta_cur_gen_df, by = "state")
theta_cur_ec %>%
    filter(is.na(est))
dem_likely <- c("Delaware", "District of Columbia", "Hawaii", "Illinois", "Oregon", "Rhode Island", "Vermont")
rep_likely <- c("Idaho", "Louisiana", "Nebraska", "North Dakota", "South Dakota", "West Virginia", "Wyoming")

write.csv(theta_cur_ec, "theta_cur_ec.csv", row.names = FALSE)

rep_votes <- sum(filter(drop_na(theta_cur_ec), est < 0)$electoralvotes) 
#+ sum(filter(theta_cur_ec, state %in% rep_likely)$electoralvotes)

dem_votes <- sum(filter(drop_na(theta_cur_ec), est > 0)$electoralvotes) 
#+ sum(filter(theta_cur_ec, state %in% dem_likely)$electoralvotes)

filter(drop_na(theta_cur_ec), est < 0)$state
filter(drop_na(theta_cur_ec), est > 0)$state

theta_cur_ec$candidate <- ifelse(theta_cur_ec$est > 0, "Biden", "Trump")

ggplot(theta_cur_gen_df, aes(x=state_gen)) +
    geom_hline(yintercept = 0, color = "grey75") +
    geom_point(aes(y = est)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
    # geom_point(aes(y = res), color = "blue", shape = 17, size = 2) +
    labs(x = "State", y = expression(hat(theta)), 
         title = "Log-ratio estimates: if the election were held today") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(theta_cur_gen_df, aes(x=state_gen)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_hline(yintercept = 10, color = "seagreen4") +
    geom_hline(yintercept = -10, color = "seagreen4") +
    geom_point(aes(y = to_spread(est))) +
    geom_errorbar(aes(ymin = to_spread(lb), ymax = to_spread(ub)), width = 0.2) +
    # geom_point(aes(y = res), color = "blue", shape = 17, size = 2) +
    labs(x = "State", y = "Biden - Trump", 
         title = "Spread estimates: if the election were held today") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Get forecasts for election day
sme_gen <- sme(object = polls_re$logratio, tme = polls_re$days_til_elec_mid, ind = polls_re$state, lambda.mu = lam_mu_use, lambda.v = lam_nu_use)
plot(sme_gen, showConfidenceBands = TRUE)
theta_0_gen <- rep(0, length(state_gen))
for (k in 1:length(state_gen)){
    theta_0_gen[k] <- spline(sort(unique(sme_gen$data$tme)), sme_gen$coefficients["mu",] + sme_gen$coefficients[paste("v", state_gen[k], sep=""),], xout=4, method = "natural")$y
}

theta_0_gen
spread_0_gen <- to_spread(theta_0_gen) # lolol

