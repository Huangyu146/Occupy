
library(openxlsx)
file_path <- "Nest.xlsx"
data <- read.xlsx(file_path, sheet = 1) 

library(data.table)
dt <- as.data.table(data)
stats <- dt[, .(
  nest_h_mean = mean(nest_h, na.rm = TRUE),nest_h_sd = sd(nest_h, na.rm = TRUE),
  nest_w_mean = mean(nest_w, na.rm = TRUE),nest_w_sd = sd(nest_w, na.rm = TRUE), 
  nest_v_mean = mean(nest_v, na.rm = TRUE), nest_v_sd = sd(nest_v, na.rm = TRUE),
  nest_bt_mean = mean(nest_b_t, na.rm = TRUE), nest_bt_sd = sd(nest_b_t, na.rm = TRUE)
), by = FD]
print(stats)

#nest hight
shapiro.test(data$nest_h[data$FD == 0]) 
shapiro.test(data$nest_h[data$FD == 1])
t.test(nest_h ~ FD, data = data)
#nest width
shapiro.test(data$nest_w[data$FD == 0]) 
shapiro.test(data$nest_w[data$FD == 1])
t.test(nest_w ~ FD, data = data)
#nest volume
shapiro.test(data$nest_v[data$FD == 0]) 
shapiro.test(data$nest_v[data$FD == 1])
wilcox.test(nest_v ~ FD, data = data)
#nest bottom thickness
shapiro.test(data$nest_b_t[data$FD == 0]) 
shapiro.test(data$nest_b_t[data$FD == 1])
wilcox.test(nest_b_t ~ FD, data = data)

