# Multi Period, Multi Group and Variation in Treatment Timing
# negative weight 
library(ggplot2)
library(dplyr)
library(tidyr)

# Example 0
# Homogeneous treatment effects (across time and unit)
reps  <- 99
set.seed  <- 1259
store  <- rep(0, reps)
store_main  <- rep(0, 8)
ind  <- 1
N <- 5000
T <- 20

# Function to generate skeleton data
datagen <- function(T, N, group){
  timeT <- rep(1:T, each = N)
  treatT <- rep(1, length(timeT))
  groupT <- rep(group, length(timeT))
  df <- data.frame(time = timeT, treat = treatT, group = groupT)
  return(df)
}

# skeleton data
dftreat <- datagen(T, N, 1) # early treatment
dftreat2 <- datagen(T, N, 2) # late treatment
dfuntreat <- datagen(T, N, 3) # untreated

data <- rbind(dftreat, dftreat2, dfuntreat)


m  <- 1
for(j in 17:10){
    for(i in 1:reps){
        trueeffect <- 10
        intercep <- 10
        early <- 10
        late <- 17

# generating policy variables 
        data <- data %>% 
            mutate(policy = 0, policy = ifelse(group == 1 & time >= early, 1, policy), 
            policy = ifelse(group == 2 & time >= late, 1, policy),
            dumtreat1 = ifelse(group == 1, 1, 0), dumtreat2 = ifelse(group == 2, 1, 0), dumtreat3 = ifelse(group == 3, 1, 0))

# error 
        e <- rnorm(nrow(data), 0, 5)

# generate the true Y, Y for simulation
        data <- data %>% mutate(
            Ytrue = 1 + trueeffect*dumtreat1*policy + trueeffect*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4, 
            Y = 1 + trueeffect*dumtreat1*policy + trueeffect*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4 + e,
            Ypot = 1 + 1*dumtreat1*policy + 1*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4 + e)

# aggregate
        datasum <- data.frame(data %>%
            group_by(group, time) %>%
            summarise(meanY_true = mean(Ytrue), 
            meanY = mean(Y), meanYpot = mean(Ypot))
        )

# make the plot of true Y
        if(ind == 1){
            vlines <- data.frame(xint = c(early, late))

            datasum$group = factor(datasum$group)
            true_plot  <- ggplot(datasum, aes(x = time, y = meanY_true, group = group)) + geom_line(aes(linetype = group, color = group),size = 2) + #geom_point(aes(shape = group, size = 3)) + 
            scale_linetype_manual(name = "Linetype",values = c("1" = 1, "2" = 1, "3" = 1, "10" = 2, "20" = 2, "30" = 2), guide = "none") + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(color = "black"), 
            legend.position = c(0.2, 0.8), text = element_text(size = 13)) + xlab("period") + ylab("Outcome") + 
            geom_vline(data = vlines, aes(xintercept = xint), linetype = "dashed") + 
            annotate(x = c(10.2, 17.2, 5, 12.5, 18), y = c(0, 0, 35, 35, 35), label = c(bquote(t[k]^"*"), bquote(t[l]^"*"), "PRE(k)", "MID(k,l)", "POST(l)"), geom = "text", parse = TRUE) + ggtitle("TRUE DGP") + theme(plot.title = element_text(face = "bold")) + scale_color_manual(name = "group", values = c(1, 2, 3), labels = c("early", "late", "never treated"))
            }else{
                print(j)
        }

        reg  <- summary(lm(Y ~ policy + factor(group) + factor(time), data = data))
        store[i]  <- coefficients(reg)[2]
        ind  <- ind + 1
    }
store_main[m]  <- mean(store)
m  <- m + 1
}

# plot
true_plot
ggsave(filename = "~/Dropbox/cigtaxes_new/output/DGP_homogeneous.pdf")

twfe  <- paste("theta == ", 2)
# plot the TWFE coefficients from the simulations
dat  <- data.frame(time = 17:10, TWFE = store_main)
ggplot(dat, aes(x = time, y = TWFE)) + geom_point(size = 3) + 
xlab("treatment time of late treated group") + ylab("TWFE Estimate") + theme_classic() + 
ylim(c(0, 20)) + ggtitle("Homogeneous Treatment Effect \n (Across Unit and Time)") + 
theme(plot.title = element_text(face="bold"), text = element_text(size = 13)) + geom_hline(yintercept = 10, 
color = "red", lty = "dashed") + annotate("text", x = 10, y = 5, label = twfe)
ggsave(filename = "~/Dropbox/cigtaxes_new/output/sim_homogeneous.pdf")




# Example 1
# Heterogeneous treatment effect by unit but treatment does not vary by time

reps  <- 99
set.seed  <- 1259
store  <- rep(0, reps)
store_main  <- rep(0, 8)
ind  <- 1

# Function to generate skeleton data
datagen <- function(T, N, group){
  timeT <- rep(1:T, each = N)
  treatT <- rep(1, length(timeT))
  groupT <- rep(group, length(timeT))
  df <- data.frame(time = timeT, treat = treatT, group = groupT)
  return(df)
}

# skeleton data
dftreat <- datagen(T, N, 1) # early treatment
dftreat2 <- datagen(T, N, 2) # late treatment
dfuntreat <- datagen(T, N, 3) # untreated

data <- rbind(dftreat, dftreat2, dfuntreat)


m  <- 1
for(j in 17:10){
    for(i in 1:reps){
        trueeffect1 <- 10
        trueeffect2 <- 5 
        intercep <- 10
        N <- 5000
        T <- 20
        early <- 10
        late <- j

# generating policy variables 
        data <- data %>% 
            mutate(policy = 0, policy = ifelse(group == 1 & time >= early, 1, policy), 
            policy = ifelse(group == 2 & time >= late, 1, policy),
            dumtreat1 = ifelse(group == 1, 1, 0), dumtreat2 = ifelse(group == 2, 1, 0), dumtreat3 = ifelse(group == 3, 1, 0))

# error 
        e <- rnorm(nrow(data), 0, 5)

# generate the true Y, Y for simulation
        data <- data %>% mutate(
            Ytrue = 1 + trueeffect1*dumtreat1*policy + trueeffect2*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4, 
            Y = 1 + trueeffect1*dumtreat1*policy + trueeffect2*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4 + e,
            Ypot = 1 + 1*dumtreat1*policy + 1*dumtreat2*policy + time + dumtreat1*2 + dumtreat2*4 + e)

# aggregate
        datasum <- data.frame(data %>%
            group_by(group, time) %>%
            summarise(meanY_true = mean(Ytrue), 
            meanY = mean(Y), meanYpot = mean(Ypot))
        )

# make the plot of true Y
        if(ind == 1){
            vlines <- data.frame(xint = c(early, late))

            datasum$group = factor(datasum$group)
            true_plotH  <- ggplot(datasum, aes(x = time, y = meanY_true, group = group)) + geom_line(aes(linetype = group, color = group),size = 2) + #geom_point(aes(shape = group, size = 3)) + 
            scale_linetype_manual(name = "Linetype",values = c("1" = 1, "2" = 1, "3" = 1, "10" = 2, "20" = 2, "30" = 2), guide = "none") + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(color = "black"), legend.position = c(0.2, 0.8), text = element_text(size = 13)) + xlab("period") + ylab("Outcome") + 
            geom_vline(data = vlines, aes(xintercept = xint), linetype = "dashed") + 
            annotate(x = c(10.2, 17.2, 5, 12.5, 18), y = c(0, 0, 35, 35, 35), label = c(bquote(t[k]^"*"), bquote(t[l]^"*"), "PRE(k)", "MID(k,l)", "POST(l)"), geom = "text", parse = TRUE) + scale_color_manual(name = "group", values = c(1,2,3), labels = c("early", "late", "never treated"))
            }else{
                print(j)
        }

        reg  <- summary(lm(Y ~ policy + factor(group) + factor(time), data = data))
        store[i]  <- coefficients(reg)[2]
        ind  <- ind + 1
    }
store_main[m]  <- mean(store)
m  <- m + 1
}

true_plotH
ggsave(filename = "~/Dropbox/cigtaxes_new/output/DGP_heterogeneous.pdf")

# plot the TWFE coefficients from the simulations
dat  <- data.frame(time = 17:10, TWFE = store_main)
ggplot(dat, aes(x = time, y = TWFE)) + geom_point(size = 3) + 
xlab("treatment timing of late group") + ylab("TWFE Estimate") + theme_classic() + 
ylim(c(5, 10)) + ggtitle("Heterogeneous Treatment Effect \n (By Unit)") + 
theme(plot.title = element_text(face="bold")) + geom_hline(yintercept = mean(c(trueeffect1, trueeffect2)), 
color = "red", lty = "dashed") + annotate(x = 14, y = 7.3, label = "mean treatment effect", color = "red", geom = "text")
ggsave(filename = "~/Dropbox/cigtaxes_new/output/sim_heterogeneous.pdf")



# Example 2 

# Heterogeneous effects over time
N  <- 3 
T  <- 21
t_early  <- 7
t_late  <-  14
X  <- seq(0, 20) 

Y0 <- -1/4 * X + 20  

Y1a  <- -1/4 * X[1:t_early] + 20 
c  <- Y1a[t_early] + 1/2 * X[t_early] 
Y1b  <- -1/2 * X[(t_early + 1):T] + c 
Y1  <- c(Y1a, Y1b)

Y2a  <- -1/4 * X[1:t_late] + 20 
c2  <- Y2a[t_late] + 1/2 * X[t_late] 
Y2b  <- -1/2 * X[(t_late + 1):T] + c2 
Y2  <- c(Y2a, Y2b)


Y0  <- data.frame(y = Y0, group = "untreated", time = 1:T) 
Y1  <- data.frame(y = Y1, group = "early treat", time = 1:T) 
Y2  <- data.frame(y = Y2, group = "late treat", time = 1:T) 
Y  <- rbind(Y0, Y1, Y2)


head(Y)

ggplot(dat = Y, aes(y = y, x = time, color = group)) + 
    geom_point(aes(shape = group), alpha = 0.6, size = 3) + 
    theme_classic() + 
    geom_vline(xintercept = c(t_early, t_late), lty = "dashed") + 
    ylab("Outcome") +
    annotate(x = c(7.4, 14.4, 5, 10.5, 16), y = c(10, 10, 22, 22, 22), label = c(bquote(t[k]^"*"), bquote(t[l]^"*"), "PRE(k)", "MID(k,l)", "POST(l)"), geom = "text", parse = TRUE) + theme(text = element_text(size = 13), legend.position = c(0.13, 0.17))
ggsave(filename = "~/Dropbox/cigtaxes_new/output/DGP_heterogeneous_time.pdf")





# using actual data
datapath  <- "~/Dropbox/cigtaxes_new/data"
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

# start and year year of the sample
start_year <- min(dat$year) + 1
end_year <- max(dat$year)


table(dat$state_abb)
table(dat$state_abb[dat$year_change_per == start_year])
length(table(dat$state_abb[dat$treat == 1]))

# MSAs that changed the tax in the second year
for(k in 0:1){
nam_msa  <- as.numeric(names(table(dat$msa_id[dat$year_change_per == start_year + k])))

D_ihat  <- mean(dat$tax_change[dat$msa_id == nam_msa[1]]) # mean for unit i (unit changing tax)
D_tgrand  <- mean(dat$tax_change) # grand mean

store  <- matrix(NA, ncol = 4, nrow = end_year - (start_year + k) + 1)

j  <- 1

for(i in (start_year+k):end_year) {
    D_that  <- mean(dat$tax_change[dat$year == i])
    D_hat  <- D_ihat + D_that - D_tgrand
    store[j, 1]  <- i 
    store[j, 2]  <- D_that 
    store[j, 3]  <- D_hat
    store[j, 4]  <- 1 - D_hat

j  <-  j + 1
}

store  <-  data.frame(store)
colnames(store)  <-  c("year", "D_that", "D_hat", "weight")
if(k == 0){
    store1  <- store
    store1$group  <- as.factor(start_year + k)
}else{
    store2  <- store
    store2$group  <- as.factor(start_year + k)
}

}

store  <-  rbind(store1, store2)

ggplot(store, aes(year, weight, color = group)) + geom_point(size = 2.5) + 
    geom_hline(yintercept = 0, lty = "dashed", size = 1, color = "black") + 
    theme_classic()


reg1  <- lm(tax_change ~ factor(year) + factor(msa_id), dat)
dat$pred  <- predict(reg1)
dat$D_hat  <- dat$tax_change - dat$pred
