#------- Uncomment to install packages-------

#install.packages(tidyverse) 
#install.packages(readxl) 
#-------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)

#----------------------------------Reading data from the excel files----------------------------------
data_path <- "C:/Users/ab251/Desktop/College/Fall19/Value_Files/Technology_Values.xlsx" #change path as needed
intrate_path <- "C:/Users/ab251/Desktop/College/Fall19/Value_Files/DFF.csv"
i <- excel_sheets(path = data_path)
List_of_Stocks <- read_excel(path = data_path, sheet = "List_of_Stocks")
MarketCap <- read_excel(path = data_path, sheet = "MarketCap")
Shares <- read_excel(path = data_path, sheet = "Shares")
Book_Value_Per_Share <- read_excel(path = data_path, sheet = "Book_Value_Per_Share")
Earnings_Per_Share <- read_excel(path = data_path, sheet = "Earnings_Per_Share")
Cost_of_Revenue <- read_excel(path = data_path, sheet = "Cost_of_Revenue")
Debt_to_Equity <- read_excel(path = data_path, sheet = "Debt_to_Equity")
Volatility <- read_excel(path = data_path, sheet = "Volatility")
Cash_Flow <- read_excel(path = data_path, sheet = "Cash_Flow")
Interest_Rate <- read_csv(intrate_path)


#have to read in interest rate file and Implied volatilty file. Use read_csv instead of read_excel
#-----------------------------------------------------------------------------------------------------
# To Consider a subset of our data from Dec 2007 to Jul 2009. Housing Bubble

DATE1 <- as.Date("2019-01-01")
DATE2 <- as.Date("2019-10-31")
datelen = as.numeric(DATE2 - DATE1)
datelen
myfunc <- function(file, x,y){file[file$End_Date >= x & file$End_Date <= y,]}
myfunc2 <- function(file, x,y){file[file$DATE >= x & file$DATE <= y,]}


names(MarketCap)[names(MarketCap) == "Start Date"] <- "Start_Date"
names(MarketCap)[names(MarketCap) == "End Date"] <- "End_Date"
MarketCap_subset <- myfunc(MarketCap, DATE1,DATE2)
MarketCap_subset[order(as.Date(MarketCap_subset$End_Date)),]

names(Shares)[names(Shares) == "Start Date"] <- "Start_Date"
names(Shares)[names(Shares) == "End Date"] <- "End_Date"
Shares_subset <- myfunc(Shares, DATE1,DATE2)
Shares_subset[order(as.Date(Shares_subset$End_Date)),]

names(Book_Value_Per_Share)[names(Book_Value_Per_Share) == "Start Date"] <- "Start_Date"
names(Book_Value_Per_Share)[names(Book_Value_Per_Share) == "End Date"] <- "End_Date"
Book_Value_Per_Share_subset <- myfunc(Book_Value_Per_Share, DATE1,DATE2)
Book_Value_Per_Share_subset <- Book_Value_Per_Share_subset[order(as.Date(Book_Value_Per_Share_subset$End_Date)),]

names(Earnings_Per_Share)[names(Earnings_Per_Share) == "Start Date"] <- "Start_Date"
names(Earnings_Per_Share)[names(Earnings_Per_Share) == "End Date"] <- "End_Date"
Earnings_Per_Share_subset <- myfunc(Earnings_Per_Share, DATE1,DATE2)
Earnings_Per_Share_subset<-Earnings_Per_Share_subset[order(as.Date(Earnings_Per_Share_subset$End_Date)),]


names(Cost_of_Revenue)[names(Cost_of_Revenue) == "Start Date"] <- "Start_Date"
names(Cost_of_Revenue)[names(Cost_of_Revenue) == "End Date"] <- "End_Date"
Cost_of_Revenue_subset <- myfunc(Cost_of_Revenue, DATE1,DATE2)
Cost_of_Revenue_subset<-Cost_of_Revenue_subset[order(as.Date(Cost_of_Revenue_subset$End_Date)),]


names(Debt_to_Equity)[names(Debt_to_Equity) == "Start Date"] <- "Start_Date"
names(Debt_to_Equity)[names(Debt_to_Equity) == "End Date"] <- "End_Date"
Debt_to_Equity_subset <- myfunc(Debt_to_Equity, DATE1,DATE2)    
Debt_to_Equity_subset<-Debt_to_Equity_subset[order(as.Date(Debt_to_Equity_subset$End_Date)),]

names(Volatility)[names(Volatility) == "Start Date"] <- "Start_Date"
names(Volatility)[names(Volatility) == "End Date"] <- "End_Date"
Volatility_subset <- myfunc(Volatility, DATE1,DATE2)    
Volatility_subset<-Volatility_subset[order(as.Date(Volatility_subset$End_Date)),]



names(Cash_Flow)[names(Cash_Flow) == "Start Date"] <- "Start_Date"
names(Cash_Flow)[names(Cash_Flow) == "End Date"] <- "End_Date"
Cash_Flow_subset <- myfunc(Cash_Flow, DATE1,DATE2)
Cash_Flow_subset<-Cash_Flow_subset[order(as.Date(Cash_Flow_subset$End_Date)),]


names(Interest_Rate)[names(Interest_Rate) == "Start Date"] <- "Start_Date"
names(Interest_Rate)[names(Interest_Rate) == "End Date"] <- "End_Date"
Interest_rate_subset <- myfunc2(Interest_Rate, DATE1, DATE2)
Interest_rate_subset<-Interest_rate_subset[order(as.Date(Interest_rate_subset$End_Date)),]


#-------------------------------------------------------------------------------------
#Calculating r* value for every time slice in the subset

#code for 1 stock ONLY! not the model. This gives intuition and fancy plots
#Interesting ones:
#stock 3 (!!)
#stock 13 
#stock 15 (?)
#stock 29
#stock 55
#stock 200 (!!)

rstar = c()
rstarind = c()
zeta = 6
stock = 29 #adjust this number (3-201) to change stock 
rad = c()
irindex = 2
c0 = c()
T_val = c()
length(4:datelen)
deltaGstarj = c()

for (index in 4:datelen) { #for loop to run through every time slice 
  Bj1 <- abs(as.numeric(Shares_subset[index, stock]))
  Bj2 <- abs(as.numeric(Book_Value_Per_Share[index, stock]))
  Bj <- Bj1*Bj2
  Vj = abs(as.numeric(MarketCap_subset[index, stock]))
  rad <- c(rad, (Vj)^(1/3))
  Cj = abs(as.numeric(Cost_of_Revenue_subset[index, stock]))
  nuj = abs(as.numeric(Volatility_subset[index, stock]))
  Fj = abs(as.numeric(Cash_Flow_subset[index, stock]))
  IR = abs(as.numeric(Interest_rate_subset[index, 2], na.rm = TRUE))
  gammaj = as.numeric((Bj/Vj)*((Cj)^(1/3))) 
  GVj = (nuj)*(Fj/Vj)
  r = (zeta)*(gammaj/GVj)
  rstar <- c(rstar, r)
  gstartemp = b*((gammaj^3)/(GVj^2))
  deltaGstarj <- c(deltaGstarj, gstartemp, na.rm = TRUE)
  temp = (r^3)/(Vj^2)
  c0 <- c(c0, temp)
  #Checking if stock is in bubble
  if(abs(Vj^(1/3) - r) > Vj^(1/3)){
    print(paste("In bubble", "Vj^(1/3):", Vj^(1/3), "r*:", r, "date:", MarketCap_subset$End_Date[index]))
  }
  temporary = Vj/(1+IR)
  T_val = c(T_val, temporary, na.rm = TRUE)
}

drdt = Vj^(1/3)

f = 0.000003/datelen


length(deltaGstarj)
length(T_val)

e = 2.71
Ndot = f*c0*e^(-(deltaGstarj)/T_val)

Gdot = mean(drdt, na.rm = TRUE)

#----------Plot for the entire time frame!
y1 = 1- e^(-(Gdot^6)*(Ndot[1:datelen]^2)*(x[1:datelen]^4))
y2 = 1- e^(-0.33*0.000005*3.14*(x[1:datelen]^4))
t = 1:datelen
df = data.frame(t, y1[1:datelen], y2[1:datelen])
ggplot(df, aes(t),  colour = "variables") +
  ggtitle("Behavior of ATVI from Jan 2018 - Oct 2019")+
  geom_line(aes(y = y1, colour = "Financial"), size = 0.5) +
  geom_line(aes(y = y2, colour = "Physical"), size = 2)


#-------- Zoomed version of the plot
y1 = 1- e^(-(Gdot^6)*(Ndot[1:400]^2)*(x[1:400]^4))
y2 = 1- e^(-0.33*0.000005*3.14*(x[1:400]^4))
t = 1:400
df = data.frame(t, y1[1:180], y2[1:180])
ggplot(df, aes(t),  colour = "variables") +
  ggtitle("Behavior of ATVI from Jan 2018 - Oct 2019")+
  geom_line(aes(y = y1, colour = "Financial"), size = 0.5) +
  geom_line(aes(y = y2, colour = "Physical"), size = 2)

cor(y1, y2)
pererror = mean(100*abs(y2-y1)/y2)







#-----------------------------------------------------------------------------------------
#plotting r* value against dates. Look at notes for interpretation!
x = 1:datelen
df = data.frame(x[1:400], rstar[1:400], rad[1:400])
ggplot(df, aes(x[1:400])) +
  ggtitle("Comparison of r* and prepice of ATVI Jan 2018 - Oct 2019")+
  geom_line(aes(y = rstar[1:400], colour = "r*"), size = 1) +
  geom_line(aes(y = rad[1:400], color = "Prepice"), size = 1)
#----------------------------------------------------------------------------------------

#The model:
zeta = 7
b = 1
#calculating r*_i for every time slice.
rstarind = c() #the average industry r* value over time (r*_i for every time slice)
rstart = c() #rstar for every stock over all time (list of lists)
Vilist =c() #dummy list to collect market cap of all stocks in a certain time
Vi = c() #market cap of industry (sum of elements of Vilist) in a certain time
deltaGstarj = c() #delta G* for a company
deltaGstari = c() #delta G* for an industry
T_val = c() #T for each time slice (goes into Ndot)
gstartemp = 0 #dummy variable to calculate delta G*
ircol = 2 #column value for interest rate
drdt = c() #change in prepice
Vbeta = c()
Vbetaind = c()

for (index in 4:datelen) { #for loop to run through every time slice
  rstar <- c() #rstar value for every stock in a given time slice
  Vbeta <- c() #To store Vbeta_j at every time slice
  for (stock in 4:203) { #for loop to run through every stock
    Bj1 <- abs(as.numeric(Shares_subset[index, stock], na.rm = TRUE))
    Bj2 <- abs(as.numeric(Book_Value_Per_Share_subset[index, stock], na.rm = TRUE ))
    Bj <- Bj1*Bj2
    Vj = abs(as.numeric(MarketCap_subset[index, stock], na.rm = TRUE))
    Vilist <- c(Vilist, Vj)
    Cj = abs(as.numeric(Cost_of_Revenue_subset[index, stock], na.rm = TRUE))
    nuj = abs(as.numeric(Volatility_subset[index, stock], na.rm = TRUE))
    Fj = abs(as.numeric(Cash_Flow_subset[index, stock], na.rm = TRUE))
    gammaj = as.numeric((Bj/Vj)*((Cj)^(1/3))) 
    GVj = (nuj)*(Fj/Vj)
    gstartemp = b*((gammaj^3)/(0.1+GVj^2))
    deltaGstarj <- c(deltaGstarj, gstartemp, na.rm = TRUE)
    r = (zeta)*(gammaj/0.1+GVj) #computation of r* for that stock
    rstar <- c(rstar, r) 
    if(abs(r) > Vj^(1/3)){
      Vbeta <- c(Vbeta, Vj) #if company is in bubble, add its marketcap to Vbeta
    }
    
  }
  Vbetaind <- c(Vbetaind, sum(Vbeta)) #add and store total market cap of companies in bubble at a certain time slice
  v = sum(Vilist, na.rm = TRUE) #dummy variable to add up all the market cap of stocks at a certain time slice
  Vi <- c(Vi, v) #appending v to Vi in order to move on to next time slice
  temporary = Vi[index-4]/(1+as.numeric(Interest_rate_subset[index, ircol], na.rm = TRUE)) #dummy variable to compute Vi/1+r
  T_val = c(T_val, temporary) #storing this into T array
  g = mean(deltaGstarj, na.rm = TRUE) #dummy variable to get delta G* for the industry at certain time slice
  deltaGstari <- c(deltaGstari, g) #delta G* for the industry at every time slice
  rstari = mean(rstar, na.rm = TRUE) #average r* for the industry
  rstart = c(rstart, list(rstar)) #append list of r* value for every stock at every time. Used in computing n 
  rstarind <- c(rstarind, rstari) #industrial average r* value
}

drdt = Vi^(1/3)


#------------------------------------------------------------------------------------------
#change of industrial r* value with time. Look at notes for interpretation!
plot(x, rstarind[1:datelen])
plot(x, deltaGstari[1:datelen])

#------------------------------------------------------------------------------------------
#calculating n value for c0
n = c() #store n value for every time step
for (index in 1:(datelen-4)) { #read through all values of rstarind (over all time slices)
  temp = 0
  for (value in 1:198){ #read through all stocks for a given time slice 
    if(is.na(rstart[[index]][value]) == FALSE && is.na(rstarind[index]) == FALSE && rstart[[index]][value] > rstarind[index]){
      temp = temp + 1
    }
  }
  n <- c(n, temp)
}


rstarind[1]^3
c0

length(n)
temp = 0
c0 = c() #n*r*i^3/Vi^2
for (i in 1:length(n)){
  temp = (n[i]*(rstarind[i]^3)) /(Vi[i]^2)
  c0 <- c(c0, temp)
}
f = 0.00005/datelen
#-------------------------------

c = mean(c0)

e = 2.71
Ndot = f*c*e^(-(deltaGstari[1:(length(T_val))])/T_val)
Ndot
Gdot = mean(drdt, na.rm = TRUE)



ratio = mean(Vbeta/Vi) #finding the average ratio between companies in and out of bubble for the theoretical model
coeff = (-log(1 - ratio))^(1/2) #finding Ndot^2Gdot^6 for theoretical model 


#coeff has value 5*10^-9


#----------Plot for the entire time frame!
y1 = 1- e^(-(Gdot^6)*(Ndot[1:datelen]^2)*(x[1:datelen]^4))
y2 = 1- e^(-0.33*coeff*3.14*(x[1:datelen]^4)) #building theoretical model using obtained coefficient
t = 1:datelen
df = data.frame(t, y1[1:datelen], y2[1:datelen])
ggplot(df, aes(t),  colour = "variables") +
  ggtitle("Behavior of Industrial entities Industry Jan 2019 - Oct 2019")+
  geom_line(aes(y = y1, colour = "Financial"), size = 1) +
  geom_line(aes(y = y2, colour = "Physical"), size = 2)

y1

#-------- Zoomed version of the plot
y1 = 1- e^(-(Gdot^6)*(Ndot[1:400]^2)*(x[1:400]^4))
y2 = 1- e^(-0.33*coeff*3.14*(x[1:400]^4))
t = 1:400
df = data.frame(t, y1[1:400], y2[1:400])
ggplot(df, aes(t),  colour = "variables") +
  ggtitle("Behavior of Industrial entity Industry Jul 2007 - Aug2008")+
  geom_line(aes(y = y1, colour = "Financial"), size = 1) +
  geom_line(aes(y = y2, colour = "Physical"), size = 2)

length(y1)

cor(y1, y2)
pererror = mean(100*abs(y2-y1)/y2)


plot(x, (1- e^(-(Gdot^3)*(Ndot[1:datelen])*(x[1:datelen]^2))))
plot(x[1:180], (1- e^(-(Gdot^3)*(Ndot[1:180])*(x[1:180]^2))))

