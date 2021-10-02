setwd('~/Documents/Decision2020/decision2020/')

library(tidyverse)
library(data.table)

to_spread <- function(x) {
    100*(exp(x)-1)/(exp(x+1))
}

final_pred <- read.csv('final_pred.csv')
final_res <- read.csv('1976-2020-president.csv')

final_pred <- final_pred %>%
    mutate(spread = to_spread(est))
# filter out CDs for now (look up and manually add to results)
final_pred_state <- final_pred %>%
    filter(!(state %like% 'CD-'))
final_pred_state %>%
    filter(tossup == 1)

final_res_2020 <- final_res %>%
    filter(year == 2020)
final_res_2020_bt <- final_res_2020 %>%
    filter(party_simplified == 'DEMOCRAT' | party_simplified == 'REPUBLICAN') %>%
    mutate(perc = (candidatevotes/totalvotes)*100)
final_res_2020_b <- final_res_2020_bt %>%
    filter(party_simplified == 'DEMOCRAT')
final_res_2020_t <- final_res_2020_bt %>%
    filter(party_simplified == 'REPUBLICAN')
final_res_spread <- final_res_2020_b$perc - final_res_2020_t$perc
final_res_df <- data.frame(state = str_to_title(final_res_2020_b$state), 
                           spread = final_res_spread) 
final_res_df$winner <- ifelse(final_res_df$spread > 0, 'Biden', 'Trump')

final_comp <- cbind(final_res_df, 
                    data.frame(spread_pred = final_pred_state$spread, 
                               winner_pred = final_pred_state$candidate))
final_comp <- final_comp %>%
    mutate(error = spread_pred - spread)

# correctly predicted winner in 47 of 51 states + DC (ignoring ME + NE CDs)
# missed AZ, FL, GA
# predicted less than 1 percentage point difference in FL (so toss-up)
final_comp %>%
    filter(winner != winner_pred)

# root mean square error = 9.67
# for *difference* between Biden and Trump percentages
sqrt(mean((final_comp$error)^2)) 

# predicted difference between Biden and Trump percentages 
# within 1 percentage point in PA, WI, WY
final_comp %>%
    filter(abs(error) < 1)



