# This file runs the TWFE and Sun & Abraham event studies 

dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

start_year <- min(dat$year)
end_year <- max(dat$year)

# a. twfe ES baseline
est_a0 <- feols(current_smoker ~ i(year_around, treat, bin = "bin::1", # nolint
                c(-1, -5)) |
                year + msa_id,
                subset(dat, year_change_per != start_year), cluster = c("fipsstatecode"), ) # nolint , year_change_per1 != j
ggiplot(est_a0)

# b. twfe ES controls
est_a1 <- feols(current_smoker ~ unemp2000:factor(year) + log_msapop00:factor(year) +  # nolint
                change90to00:factor(year) + per_barban05 +  
                i(year_around, treat, bin = "bin::1", # nolint
                c(-1, -6)) |
                year + msa_id,
                subset(dat, year_change_per != start_year), cluster = c("fipsstatecode")) # nolint , year_change_per1 != j
ggiplot(est_a1)

# for Sun and Abraham, if never_treated give value of 10000
dat <- dat %>% mutate(year_change_per2 = ifelse(treat == 0, 10000, year_change_per)) # nolint

# c. Sun & Abraham baseline
est_a2 <- feols(current_smoker ~ sunab(year_change_per2, year, ref.p = c(-1, -5) # nolint
                ) |
                year + msa_id,
                subset(dat), cluster = c("fipsstatecode")) # nolint , year_change_per1 != j

ggiplot(est_a2)

a <- dat %>% filter(fipsstatecode == 39 ) %>% arrange(msa_id, year)

# d. Sun & Abraham with controls
est_a3 <- feols(current_smoker ~ unemp2000:factor(year) +  log_msapop00:factor(year) + 
                change90to00:factor(year)+ per_barban05 +  
                sunab(year_change_per2, year, ref.p = c(-1, -6)) # nolint
                | year + msa_id,
                subset(dat), cluster = c("fipsstatecode")) # nolint , year_change_per1 != j

ggiplot(est_a3)

# TWFE and SA Event Study Estimates
f0 <- ggiplot(list('TWFE' = est_a0, 'Sun & Abraham (2020)' = est_a2),
        main = 'A. Baseline', ref.line = -1, pt.join = TRUE) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
panel.background = element_blank(),  axis.line = element_line(color = "black"),
axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1), 
legend.position = c(0.25, 0.1), text = element_text(size = 17)) +
ylab("% current smokers") + xlab("relative year")

f1 <- ggiplot(list('TWFE' = est_a1, 'Sun & Abraham (2020)' = est_a3),
        main = 'B. With Controls', ref.line = -1, pt.join = TRUE) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
panel.background = element_blank(),  axis.line = element_line(color = "black"),
axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1), 
legend.position = c(0.25, 0.1), text = element_text(size = 17)) +
ylab("% current smokers") + xlab("relative year")

ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2),
file =  file.path(outpath, "TWFE_SA_eventstudy.pdf"), height = 10, width = 6)







