library(readxl)
data.full = read_xlsx("bsg653_3.xlsx")
data = data.full[, c("AREANAME", "DIS_SAND", "ICELAST09", "WHIBIN", "BOTTOMCOV", "BOTTOM")]
# 0 : clear
# 1 : covered
data = data[data$BOTTOM!=0,]
data$BOTTOMCOV = ifelse(data$BOTTOMCOV == 5, 1, 0)

head(data)

write.csv(data, file="white_fishes_data.csv", row.names=FALSE, col.names=TRUE)

write.table(data, file="white_fishes_data.txt", row.names=FALSE, col.names=TRUE)


# data2 = read.csv("white_fishes_data.csv")
# head(data2)


# y = table(data$BOTTOM[data$WHIBIN ==1], data$BOTTOMCOV[data$WHIBIN ==1])
# N = table(data$BOTTOM, data$BOTTOMCOV)
# 
# 
# data2=cbind(data$WHIBIN,data$BOTTOMCOV)
# 
# table(data2)               
# 
