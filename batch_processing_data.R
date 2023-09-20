source("LMS_functions.R")

#This script serves as a blueprint for batch calculation. You may load, insert, and format your own data correctly for this to work.

#Creating exemplary data:
df <- data.frame(
  vertebral_level = c("L3", "T10", "T8", "T5"),
  age = c(38, 50, 72, 45),
  csma = c(200, 75, 100, 130),
  sex = c("Male", "Female", "Male", "Female")
)

#inspect that data
df

#Calculate z scores for each row by applying the get_z function
df$z <- apply(datasheet, 1, function(row) {
  get_z(x = as.numeric(row["csma"]), age = as.numeric(row["age"]), metric = "CSMA", sex = row["sex"], lvl = row["vertebral_level"])
})

#inspect results - notice the new column "z"
df

