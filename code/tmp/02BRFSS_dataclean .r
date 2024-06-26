rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(tidyselect)
library(plm)
library(ggplot2)
library(maps)
library(ggpubr)
library(gridExtra)
#install.packages("bacondecomp")
library(data.table)
library(janitor)
library(fixest)
library(bacondecomp)
library(ggiplot)
library(fuzzyjoin)
library(xtable)
library(did)


user <- 1
if (user == 1) {
        datapath <- file.path("/home/user1/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/user1/Dropbox/cigtaxes_new", "output")
}else{
        datapath <- file.path("/home/vinish/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/vinish/Dropbox/cigtaxes_new", "output")
}

datalist <- c(paste0("MMSA0", seq(2, 9, 1), ".xpt", sep = ""))
datalist <- c(datalist, paste0("MMSA", 10, ".xpt", sep = ""))
l <- list()

#############################################################
#############################################################
#
# BFRSS SMART Data Clean: 2002--2010
#
#
#############################################################
#############################################################

i <- 1
j <- 2002
for (filename in datalist) {

    dat <- data.frame(read_xpt(file.path(datapath, "BRFSS_MSA", filename))) # nolint
    dat <- dat %>%
             rename_all(., .funs = tolower)

    if (i == 3) {
        dat <- dat %>% select(-c("x_lmmsa", "sexintmn", "sexcondm"))
    }else if (i == 1) {
        dat <- dat %>% select(-c("anlmmsa"))
    }else {
        print("other")
    }

    dat <- dat %>% select(ends_with("mmsa"), contains("mmsaname"), # nolint 
                     contains("imonth"), contains("iyear"), 
                     contains("rfsmok"), contains("smoke100"), contains("smoker"), contains("stopsmk"),  # nolint
                     contains("drnkany"),   # nolint
                     contains("rfbinge"), contains("alcday"), starts_with("sex"), # nolint
                     starts_with("race2"), contains("income"), contains("employ"), "marital", "educa", ends_with("mmsawt")) %>% # nolint
                     mutate(year = j)

    a <- ncol(dat)
    print(paste0(filename, "--", a, "cols", sep = ""))
    l[[i]] <- c(colnames(dat))
    i <- i + 1

    # renaming the columns 
    colnames(dat) <- c("msa_id", "sex_msa", "age_msa", "adjmsa", "imonth", "iyear", # nolint
                     "rfsmok", "ever_smoke", "smoker", "stopsmok", # nolint
                    "drnkany", "alcday", "sex", "race2", "income2", "employ", "marital", "educa", "msawt", "year") # nolint

    write.csv(dat, paste0(datapath, "/BRFSS_MSA/brfss", j, ".csv", sep = ""))
    j <- j + 1
}



#############################################################
#############################################################
#
# BFRSS SMART Data Clean: 2014--2020
#
#
#############################################################
#############################################################
l <- list()
i <- 1
j <- 2014
datalist <- c(paste0("MMSA", seq(2014, 2020, 1), ".xpt", sep = ""))

for (filename in datalist) {

    dat <- data.frame(read_xpt(file.path(datapath, "BRFSS_MSA", filename))) # nolint
    dat <- dat %>%
             rename_all(., .funs = tolower)

    dat <- dat %>% select(ends_with("mmsa"), "x_mmsawt", # nolint 
                     contains("rfsmok"), contains("smoker"), contains("stopsmk"),  # nolint
                     contains("drnkany"),   # nolint
                     contains("rfbinge"), contains("alcday"), starts_with("sex"), # nolint
                     "x_race", contains("income"), contains("employ"), "marital", "educa") %>% # nolint
                     mutate(year = j)

    a <- ncol(dat)
    print(paste0(filename, "--", a, "cols", sep = ""))
    l[[i]] <- c(colnames(dat))
    i <- i + 1


    colnames(dat) <- c("msa_id", "msa_wt", "rfsmok", "smoker", "stopsmok", # nolint
                    "drnkany", "alcday", "sex", "race2", "income2", "employ", "marital", "educa", "year") # nolint

    write.csv(dat, paste0(datapath, "/BRFSS_MSA/brfss", j, ".csv", sep = ""))
    j <- j + 1
}


##################################################
#
# Append files for 2002-2010 and 2014-2020 brfss
#
##################################################

# 2002-2010
fun_dataconst <- function(start, end) {
    for (i in start:end){
        if (i == start)   {
            dat <- read.csv(paste0(datapath, "/BRFSS_MSA/brfss", i, ".csv", sep = "")) # nolint
        }else {
            dat_new <- read.csv(paste0(datapath, "/BRFSS_MSA/brfss", i, ".csv", sep = "")) # nolint
            dat <- rbind(dat, dat_new)
        }
    }
    return(dat)
}

#################################################
#
#  Functions
#
#################################################

fun_cigtaxtracker <- function(dat, loyear, hiyear) {
        cig_data_state <- dat %>% 
                    filter(year >= loyear & year <= hiyear) %>%
                    group_by(abb) %>%
                    summarize(min_cig_tax = min(cigtax), 
                    max_cig_tax = max(cigtax), 
                    numtax_changes = sum(tax_change, na.rm = T)) %>% 
                    mutate(tax_changeamount = max_cig_tax - min_cig_tax)  %>% # nolint
                    select(c(abb, numtax_changes, tax_changeamount))
        cig_data_state <- data.frame(cig_data_state)
        return(cig_data_state)
} 

###################################
#
# Function to construct cigarette 
# tax data
###################################

fun_taxwindow <- function(dat, year0, year1)   {
    # filter data by year of interest and tax change == 1
    cigtax_bef <- dat %>% filter(year <= year1 & year >=
                                    year0 & tax_change == 1) %>%
                          mutate(numtaxinc = 1)

    # pick the year or first year of tax change (if multiple changes) 
    index_bef <- cigtax_bef %>% 
                group_by(fips, abb) %>% # nolint
                summarize(year_change_per =  min(year), 
                tax_change_dose = mean(tax_change_dose, na.rm = T), # nolint
                numtaxinc = sum(numtaxinc))                  # nolint

    return(index_bef)
}


balanced <-function(data, ID, TIME, VARS, required=c("all","shared")) {
    if(is.character(ID)) {
        ID <- match(ID, names(data))
    }
    if(is.character(TIME)) {
        TIME <- match(TIME, names(data))
    }
    if(missing(VARS)) { 
        VARS <- setdiff(1:ncol(data), c(ID,TIME))
    } else if (is.character(VARS)) {
        VARS <- match(VARS, names(data))
    }
    required <- match.arg(required)
    idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
    timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
    complete <- complete.cases(data[, VARS])
    tbl <- table(idf[complete], timef[complete])
    if (required=="all") {
        keep <- which(rowSums(tbl==1)==ncol(tbl))
        idx <- as.numeric(idf) %in% keep
    } else if (required=="shared") {
        keep <- which(colSums(tbl==1)==nrow(tbl))
        idx <- as.numeric(timef) %in% keep
    }
    data[idx, ]
}

#################################################
#################################################
#
# Sample collapsed by MSA-level
#
#################################################
#################################################
start_year <- 2004
end_year   <- 2010
cutoff <- 0.385

quarter <- data.frame(imonth = seq(1, 12), quarter = rep(seq(1, 2), each = 6))
# constructing data
dat <- fun_dataconst(start_year, end_year) %>% 
                           filter(smoker <= 4 | ever_smoke <= 2) %>%
                           filter(age_msa != 6) %>%
                           mutate(current_smoker = ifelse(smoker <= 1, 1, 0),
                                  stop_smoking = ifelse(stopsmok == 2, 0, stopsmok), # nolint
                                  ever_smoke = ifelse(ever_smoke == 2, 0, ever_smoke)) %>% # nolint
                                  merge(quarter, by = "imonth", all.x = T) 


# collapse data into MSA using msawt
dat <- data.frame(dat %>% mutate(ind = 1) %>%
             group_by(year, msa_id) %>%
             summarize(ever_smoke = weighted.mean(ever_smoke, na.rm = T, weight = msawt), # nolint
                       current_smoker = weighted.mean(current_smoker,  na.rm = T, weight = msawt), # nolint
                       stop_smoking = weighted.mean(stop_smoking, na.rm = T, weight = msawt), N = sum(ind)) # nolint
                       ) 

# use only the balanced panel of MSAs
# dat <- make.pbalanced(dat) 

dat <- balanced(dat, "msa_id", "year")

dat <- dat %>% arrange(msa_id, year)
# dat14to20 <- fun_dataconst(2014, 2020)


#############################################################
#
# Merge with Census Core-Based Statistical Area (CBSA) 
# crosswalk.
# link: https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk # nolint
#############################################################

# cbsa to fips crosswalk file 
crosswalk <- read.csv(file.path(datapath, "cbsa_fips_crosswalk.csv"))   # 2016 crosswalk # nolint
crosswalk05 <- read.csv(file.path(datapath, "cbsatocountycrosswalk2005.csv")) # 2005 crosswalk # nolint

# crosswalk: MSA mapped to counties; contains duplicates
temp <- crosswalk
temp$dupcodes <- duplicated(crosswalk$cbsacode)  
temp <- subset(temp, dupcodes == TRUE)
temp <- temp %>% arrange(cbsacode)

#########################################
#
# Yearly change in cigarette taxes file
#
#########################################

taxes <- read.csv(file.path(datapath, "cigtax85to2014_clean.csv"))
taxes <- taxes %>% mutate(year = year - 1)

# check to see whether states passed taxes in 2002 or 2003 (over $0.5) 
# Note that 2002-2003 are pre-period for the start of the panel
taxes0203 <- data.frame(fun_taxwindow(taxes, 2002, 2003)) %>% 
                        arrange(year_change_per, tax_change_dose) %>%
                        filter(tax_change_dose >= cutoff) %>%
                        group_by(fips, abb) %>%
                        filter(n() == 1) %>%
                        select(fips) %>%
                        mutate(tax_change0203 = 1)

# taxes for the desired panel
taxes <- data.frame(fun_taxwindow(taxes, start_year, end_year)) 

# merge desired panel with tax change 02-03
taxes <- taxes %>% merge(taxes0203, by = c("fips", "abb"), all.x = T) %>%
                        mutate(tax_change0203 = ifelse(is.na(tax_change0203) == T, 0, tax_change0203))  # nolint

# collect treated states by year 
l <- list()
j <- 1
for(i in start_year:end_year) {
    l[[j]] <- table(taxes$abb[taxes$year_change_per == i])
    j <- j + 1
}


# merge dat with msa_id to fips code using the crosswalk
# make sure to exclude duplicates in crosswalk
# while merging
dat <- dat %>% merge(crosswalk[!duplicated(crosswalk$cbsacode),], by.x = "msa_id", by.y = "cbsacode", all.x = T) %>% # nolint
                           merge(taxes, by.x = c("fipsstatecode"), by.y = c("fips"), all.x = T) %>% # nolint
                           mutate(tax_change0203 = ifelse(is.na(tax_change0203) == T, 0, tax_change0203)) # nolint

#table(dat$msa_id[is.na(dat$fipsstatecode) == TRUE])

##########################################################
#
#  Fixing MSA codes that were not mapped to FIPS state
#  i) use crosswalk2005; ii) manual fix 
##########################################################
miss_msa <- data.frame(msa_id = as.numeric(names(table(dat$msa_id[is.na(dat$fipsstatecode) == TRUE]))))  # nolint
miss_msa <- miss_msa %>% merge(crosswalk05[!duplicated(crosswalk05$cbsacode),], by.x = "msa_id",  # nolint
                                by.y = "cbsacode", all.x = TRUE) %>%
                                select(c("msa_id", fipsstatecode)) %>%
                                rename(fipsstatecode2 = fipsstatecode) 

# merge msas that are unmapped with fips using miss_msa file 

dat <- dat %>%
           merge(miss_msa, by = "msa_id", all.x = TRUE) %>%
           mutate(fipsstatecode = ifelse(is.na(fipsstatecode) == T, fipsstatecode2, fipsstatecode)) %>% # nolint
           select(-c("fipsstatecode2"))   # nolint


# manually fix missing msa_id to state mapping
dat$statename[dat$msa_id == 42580] <- "Delaware" 
dat$fipsstatecode[dat$msa_id == 42580] <- 10 
dat$statename[dat$msa_id == 30100] <- "New Hampshire" 
dat$fipsstatecode[dat$msa_id == 30100] <- 33 


#########################################################
#########################################################
#
#  Setting variables for estimation
#
#
#########################################################
#########################################################

# DEFINITIONS
# year_change_per: year of the first tax change in the panel 
        # note that if year_change_per is NA, then the state is never_treated
        # in this case give it a value 0.

# year_around: This is relative time in Sun and Abraham 
        # for never_treated, year_around = 0

# treat: tracks whether the unit is treated within the desired panel

# tax_change: whether unit i is treated at time t

dat <- dat %>% 
            mutate(year_change_per = ifelse(is.na(year_change_per) == TRUE, 0, year_change_per),  # nolint
            year_around = ifelse(year_change_per != 0, year - year_change_per, 0), # nolint
            treat = ifelse(year_change_per != 0, 1, 0),  # nolint
            tax_change = ifelse(year_around >= 0, 1, 0)) # nolint


################################################################
################################################################
#
# Merging with covariates
#
################################################################
################################################################

covdat <- read.csv(file.path(datapath, "covariates.csv"))
barban <- read.csv(file.path(datapath, "sf_airlaws_bar.csv"))
msa_pop <- read.csv(file.path(datapath, "MSA_pop_clean.csv")) 

dat <- dat %>% merge(covdat, by.x = c("fipsstatecode"), 
            by.y = c("fips"), all.x = T) %>% 
            mutate(current_smoker = current_smoker * 100)

# merging with bar bans and msa population
dat <- dat %>% merge(barban, by.x = c("state_abb", "year"), by.y = c("state", "bar_year"), all.x = T) %>% # nolint
               merge(msa_pop, by.x = "msa_id", by.y = "cbsa_code", all.x = T) %>% # nolint
               select(-c(X, X.y, mmsa_name)) 
               
################################################
#
# Fixing the missing msa population manually
# use: https://s4.ad.brown.edu/Projects/Diversity/segregation2010/msa.aspx?metroid=14484 metroid = msa_id # nolint
################################################
miss <- names(table(dat$msa_id[is.na(dat$msa_pop00) == T]))

dat$msa_pop00[dat$msa_id == miss[1]] <- 1068618
dat$msa_pop10[dat$msa_id == miss[1]] <- 1170357
dat$msa_pop00[dat$msa_id == miss[2]] <- 1812937
dat$msa_pop10[dat$msa_id == miss[2]] <- 1887792
dat$msa_pop00[dat$msa_id == miss[3]] <- 1465396
dat$msa_pop10[dat$msa_id == miss[3]] <- 1480260
dat$msa_pop00[dat$msa_id == miss[4]] <- 1186999
dat$msa_pop10[dat$msa_id == miss[4]] <- 1250679
dat$msa_pop00[dat$msa_id == miss[5]] <- 3451226
dat$msa_pop10[dat$msa_id == miss[5]] <- 4235751
dat$msa_pop00[dat$msa_id == miss[6]] <- 2061162
dat$msa_pop10[dat$msa_id == miss[6]] <- 1820584
dat$msa_pop00[dat$msa_id == miss[7]] <- 2173869
dat$msa_pop10[dat$msa_id == miss[7]] <- 2340249
dat$msa_pop00[dat$msa_id == miss[8]] <- 1710318
dat$msa_pop10[dat$msa_id == miss[8]] <- 2136022
dat$msa_pop00[dat$msa_id == miss[9]] <- 9514654
dat$msa_pop10[dat$msa_id == miss[9]] <- 9818605
dat$msa_pop00[dat$msa_id == miss[10]] <- 2753913
dat$msa_pop10[dat$msa_id == miss[10]] <- 2832882
dat$msa_pop00[dat$msa_id == miss[11]] <- 2098843
dat$msa_pop10[dat$msa_id == miss[11]] <- 2147727
dat$msa_pop00[dat$msa_id == miss[12]] <- 11296377
dat$msa_pop10[dat$msa_id == miss[12]] <- 11576251
dat$msa_pop00[dat$msa_id == miss[13]] <- 3849647
dat$msa_pop10[dat$msa_id == miss[13]] <- 4008994
dat$msa_pop00[dat$msa_id == miss[14]] <- 389592
dat$msa_pop10[dat$msa_id == miss[14]] <- 418366
dat$msa_pop00[dat$msa_id == miss[15]] <-  2343058
dat$msa_pop10[dat$msa_id == miss[15]] <- 2644584
dat$msa_pop00[dat$msa_id == miss[16]] <- 700820
dat$msa_pop10[dat$msa_id == miss[16]] <- 795225
dat$msa_pop00[dat$msa_id == miss[17]] <- 2391395  
dat$msa_pop10[dat$msa_id == miss[17]] <- 2475666
dat$msa_pop00[dat$msa_id == miss[18]] <- 3727565
dat$msa_pop10[dat$msa_id == miss[18]] <- 4377008
dat$msa_pop00[dat$msa_id == miss[19]] <- 650501 
dat$msa_pop10[dat$msa_id == miss[19]] <- 705670




# states without free-standing bars are given 0.
# effects observed by st. fixed effects
dat <- dat %>% 
            mutate(per_barban00 = ifelse(is.na(per_barban00) == TRUE, 0, per_barban00), # nolint
            per_barban10 = ifelse(is.na(per_barban10) == TRUE, 0, per_barban10),
            per_barban05 = ifelse(is.na(per_barban05) == TRUE, 0, per_barban05),
            log_msapop00 = log(msa_pop00), 
            log_msapop10 = log(msa_pop10)
             )

# weights proportional to the num

write.csv(dat, file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))


#########################################
#
# Collecting States that increased tax 
# between 2004 to 2010
#########################################

b <- length(l)
store <- matrix(0, nrow = length(l), ncol = 3)
y <- 2004
for (j in 1:b) {
    a <- length(names(l[[j]]))
    for (i in 1:a) {
        if (i == 1) {
            nam <- names(l[[j]][i])
        }else {
        nam_temp <- names(l[[j]][i])
        nam <- paste(nam, nam_temp, sep = ", ")
        }
    }
    store[j, 1] <- y
    store[j, 2] <- nam
    store[j, 3] <- length(table(dat$msa_id[dat$year == y]))
    y <- y + 1
}

colnames(store) <- c("year", "states", "count MMSAs")
filetosav <- xtable(store)
print.xtable(filetosav, file = file.path(outpath, "tax_changes_msacount.tex"), include.rownames = FALSE) # nolint

