remit <- read.csv("Desktop/remit.csv")
summary(remit)
str(remit)
head(remit)
reg_score_bar <- ggplot(remit, aes(x = factor(total_policy))) +
  geom_bar(fill = "coral", 
           alpha = 0.5,) +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = 0, hjust = 0.3)
reg_index <- reg_score_bar + labs(x = "Regulation Score") + ggtitle("Figure 1") + 
  theme(plot.title = element_text(hjust = 0.5))
reg_index
table(remit$operator_auth, remit$financial_incl, remit$exclu_ban, remit$info_transparency)
exclu_bar <- ggplot(remit, aes(x = factor(exclu_ban))) +
  geom_bar(fill = "coral", 
           alpha = 0.5,) +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = 0, hjust = 0.3)
exclusivity_ban <- exclu_bar + labs(x = "Ban on Exclusivity Agreements") + ggtitle("Figure 2") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))
exclusivity_ban
operator_bar <- ggplot(remit, aes(x = factor(operator_auth))) +
  geom_bar(fill = "coral", 
           alpha = 0.5,) +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = 0, hjust = 0.3)
operator_authorization <- operator_bar + labs(x = "Operator Authorization") + ggtitle("Figure 3") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))
operator_authorization
fin_incl_bar <- ggplot(remit, aes(x = factor(financial_incl))) +
  geom_bar(fill = "coral", 
           alpha = 0.5,) +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = 0, hjust = 0.3)
financial_inclusion <-fin_incl_bar + labs(x = "Financial Inclusion") + ggtitle("Figure 4") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))
financial_inclusion
info_bar <- ggplot(remit, aes(x = factor(info_transparency))) +
  geom_bar(fill = "coral", 
           alpha = 0.5,) +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = 0, hjust = 0.3)
information <- info_bar + labs(x = "Information and Transparency") + ggtitle("Figure 5") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))
information
counts <- table(remit$operator_auth + remit$financial_incl + remit$exclu_ban + remit$info_transparency)
counts
cor.test(remit$total_policy, remit$gdp_round,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)
cor.test(remit$total_policy, remit$fragile_round,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)
cor.test(remit$total_policy, remit$public_round,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)
cor.test(remit$total_policy, remit$polity_score,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)
cor.test(remit$total_policy, remit$v_dem,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)
remit$index_gdp <- factor(remit$index_gdp, levels = c("low_dependency", "medium_dependency", "high_dependency"))
remit$index_polity <- factor(remit$index_polity, levels = c("democracy", "anocracy", "autocracy"))
remit$index_fragile <- factor(remit$index_fragile, levels = c("stable", "warning", "alert"))
remit$total_policy <- factor(remit$total_policy, levels = c("0", "1", "2", "3", "4"))
multi_index_pol_frag <- multinom(formula = total_policy ~ index_gdp + index_polity + index_fragile, 
                                 data = remit)
catdep_catpolity_catpublic <- multinom(total_policy ~ dependency + democracy + weak_capacity, data = remit)
summary(catdep_catpolity_catpublic)
catdep_catvdem_catpublic <- multinom(total_policy ~ dependency + v_dem_cat + weak_capacity, data = remit)
summary(catdep_catvdem_catpublic)
multi_index_v_pub <- multinom(formula = total_policy ~ index_gdp + index_v_dem + index_public, data = remit)
summary(multi_index_v_pub)
multi_index_pol_pub <- multinom(formula = total_policy ~ index_gdp + index_polity + index_public, data = remit)
summary(multi_index_pol_pub)