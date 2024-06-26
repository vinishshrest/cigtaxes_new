# summary statistics

dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

vars <- c("pop1990", "pop2000", "unemp1990", "unemp2000", "cig_sales_pc1990", "cig_sales_pc2000", # nolint
          "cig_sales_pc2010", "anti1998", "per_barban10", "tax_change") # nolint

datsum1t <- dat %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 1) %>%
           select(-c(tax_change)) 

datsum1u <- dat %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 0) %>%
           select(-c(tax_change)) 

datsum2t <- dat2 %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 1) %>%
           select(-c(tax_change)) 

datsum2u <- dat2 %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 0) %>%
           select(-c(tax_change)) 

fun <- function(x) {
  m <- mean(x, na.rm = T)
  s <- sd(x, na.rm = T)
  ms <- cbind(m,s)
  return(ms)
}

#for treated and untreated states
sumT <- round(sapply(datsum1t, fun), digits = 2)
sumU <- round(sapply(datsum1u, fun), digits = 2)
sumT2 <- round(sapply(datsum2t, fun), digits = 2)
sumU2 <- round(sapply(datsum2u, fun), digits = 2)

varsnam <- c("population 1990", "population 2000", "unemployment 1990", "unemployment 2000",  # nolint
            "cig sales pc (1990)", "cig sales pc (2000)", "cig sales pc (2010)",  # nolint
             "smoking sentiment (1998)", "% under bar ban")
  

store <- matrix(NA, ncol = 5, nrow = ncol(sumT) * 2)
store1 <- store
k <- 1
for(i in seq(1, nrow(store) - 1, 2)) {
j <- i + 1
#########################################
#
# For expansion states
#
#########################################
store[i,2] <- round(sumT[1, k], 2)
store[j,2] <- paste("(", round(sumT[2, k], 2), ")", sep = "")

store[i,3] <- round(sumU[1, k], 2)
store[j,3] <- paste("(", round(sumU[2, k], 2), ")", sep = "")

store[i,4] <- round(sumT2[1, k], 2)
store[j,4] <- paste("(", round(sumT2[2, k], 2), ")", sep = "")

store[i,5] <- round(sumU2[1, k], 2)
store[j,5] <- paste("(", round(sumU2[2, k], 2), ")", sep = "")

store[i, 1] <- varsnam[k]
k <- k + 1
}

colnames(store) <- c("variables", "tax change \n (04-10)", "no change \n (04-10)", # nolint
                    "tax change \n (15-20)", "no change \n (15-20)")

store <- xtable(store,  include.rownames = FALSE) 

#latex table 
print(store, include.rownames = FALSE,  paste(outpath, file = "sum_stat.tex", sep = "/"), type = "latex") # nolint

