rm(list = ls())
library(haven)
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
install.packages(patchwork)
library(gridExtra)
datapath <- "/home/user1/Dropbox/cigtaxes_new/data"

cig_data <- read_dta(paste(datapath, "cigprice_tax1985to2014.dta", sep = "/"))
cig_data$year <- as.numeric(as.character(cig_data$year))
cig_data <- pdata.frame(cig_data, index = c("fips", "year"))
cig_data$cigtax <- round(cig_data$cigtax, 3)
cig_data <- cig_data %>% group_by(fips) %>% mutate(lagcigtax = dplyr::lag(cigtax))



cig_data <- cig_data %>% mutate(tax_change_dose = cigtax - lagcigtax,
                                tax_change = ifelse(tax_change_dose != 0, 1, 0))


for(i in 1:30) {
    if(i == 1){
    cig_data <- cig_data %>%
    group_by(fips) %>%
    mutate(taxchangelag = dplyr::lag(tax_change),
    taxchangelag = ifelse(is.na(taxchangelag) == TRUE, 0, taxchangelag),
    numer_change = tax_change + taxchangelag)
    }
    else{
    cig_data <- cig_data %>% 
    group_by(fips) %>% 
    mutate(numchangelag = dplyr::lag(numer_change))
    cig_data <- cig_data %>%
    mutate(numchangelag = ifelse(is.na(numchangelag) == TRUE, 0, numchangelag))
    cig_data <- cig_data %>% 
    group_by(fips) %>% 
    mutate(numer_change = tax_change + numchangelag)
    }
}

cig_data <- cig_data %>% select(-c(taxchangelag, numchangelag))
cig_data$year <- as.numeric(as.character(cig_data$year))
cig_data$fips <-  as.numeric(as.character(cig_data$fips))
#View(cig_data)

data(state.fips)
state.fips <- state.fips %>% select(c(fips, abb))
state.fips <- rbind(state.fips, c(15, "HW"), c(2, "AK"))
cig_data <- merge(cig_data, state.fips, by = "fips", all.x = T)

# Plotting the number of times tax changed
vlines <- data.frame(xint = c(1998, 2008), grp = c(1, 2))

# need to put it in a function
f0 <- ggplot(subset(cig_data, fips >= 1 & fips <= 28) , aes(x = year, y = numer_change)) +
geom_line(size = 1) + geom_vline(data = vlines, aes(xintercept = xint, colour = grp), linetype = "dashed") + 
facet_wrap(~ abb) + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none") + 
scale_x_continuous(breaks = seq(from = 1986, to = 2014, by = 4)) + ylab("number of tax changes (1985-2014)")

f1 <- ggplot(subset(cig_data, fips >= 29 & fips <= 60) , aes(x = year, y = numer_change)) +
geom_line(size = 1) + geom_vline(data = vlines, aes(xintercept = xint, colour = grp), linetype = "dashed") + 
facet_wrap(~ abb) + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none") + 
scale_x_continuous(breaks = seq(from = 1986, to = 2014, by = 4)) + ylab("number of tax changes (1985-2014)")

ggsave