source("nicefigs.R")

hour    <- rep(c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                 "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"), each = 2)
minutes <- rep(c("00", "30"), 24)
tday <- paste(hour, minutes, sep=":")

abbr.dweek <- c("Mon","Tue","Wed","Thu","Fri", "Sat","Sun")

taus <- rev(seq(1, 99)/100)

hours_night <- night_hours <-  c(seq(1, 12), 46, 47, 48)
hours_day   <- day_hours <- setdiff(seq(1, 48), hours_night)
hours_all <- c(hours_night, hours_day)
index_hours <- sort(hours_all, index = T)$ix

iday_withmodels <- c(1, seq(10, 90, 10))

#n_past_obs_kd    <- 90 *48
n_past_obs_kd    <- 60 *48
n_past_obs_tbats <- (31 + 28 + 31 + 30)*48

M <- 120 * 48 # 5760
  
q_probs <- seq(M)/(M + 1)

m_1 <- 48
m_2 <- 336

algo.agg <- "DETS"
algo.bottom <- "KD-IC-NML"

min_bandwith <- 10^-6

print("----")
print(algo.agg)
print(algo.bottom)
print("----")

