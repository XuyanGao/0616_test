library(readr)
library(rstudioapi)

# Set working directory to path of the script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#residential_FAC=Coefficient_1-3.42log10(UPD)+0.00074GDPC
#commercial_FAC=Coefficient_2-1.59log10(UPD)+0.00011GDPC
data <- read.csv("../data/R_test_data.csv",header = TRUE)


View(data)
row.names(data)<-data$Code
#Coefficient_1<-data$R_coefficient
#Coefficient_2<-data$C_coefficient
#define the row number of Code
a<-"CHN"
R_FAC=function(a){
  r=grep(a, data$Code)
  Coefficient_1=data[r,5]
  #Coefficient_2=data[r,6]
  r_UPD=data[r,3]
  r_GDPC=data[r,4]
  R_result=Coefficient_1-3.42*log10(r_UPD)+0.00074*r_GDPC
  return(R_result)
}

C_FAC=function(a){
  r=grep(a, data$Code)
  #Coefficient_1=data[r,5]
  Coefficient_2=data[r,6]
  r_UPD=data[r,3]
  r_GDPC=data[r,4]
  C_result=Coefficient_2-1.59*log10(r_UPD)+0.00011*r_GDPC
  return(C_result)
}

R_FAC(a)
C_FAC(a)
paste(a,"Residential_FAC",R_FAC(a))
paste(a,"Commercial_FAC",C_FAC(a))

r<-grep(a, data$Code)
Coefficient_1<-data[r,5]
Coefficient_2<-data[r,6]
r_UPD<-data[r,3]
r_GDPC<-data[r,4]
residential_FAC<-Coefficient_1-3.42*log10(r_UPD)+0.00074*r_GDPC
commercial_FAC<-Coefficient_2-1.59*log10(r_UPD)+0.00011*r_GDPC
paste(a,"Residential_FAC",residential_FAC)
paste(a,"Commercial_FAC",commercial_FAC)

#z-score standardization
dt<-as.matrix(scale(data[,3:6]))
head(dt)
#correlation coefficient matrix
rm1<-cor(dt)
rm1