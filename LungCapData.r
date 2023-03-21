
#read data from excel
library(readxl)
LungCapData <- read_excel("C:/Users/sujeewae/Downloads/New folder/LungCapData.xls", 
                          col_types = c("numeric", "skip", "numeric", 
                                        "text", "text", "text"), na = "***")
#View data
View(LungCapData)
#check dimensions of the object.
dim(LungCapData)
#to view the first few rows of a data frame
head(LungCapData)
#to view the last few rows of a data frame
tail(LungCapData)