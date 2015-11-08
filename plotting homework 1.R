Plot1<-function{
  housepower<- read.table("household_power_consumption.txt", as.is=TRUE, header = TRUE, sep=";" )
  
  a<-strptime(housepower[,1],format="%d/%m/%Y")
  b<-a=="2007-02-01 00:00:00"
  c<-a=="2007-02-02 00:00:00"
  d<-b|c
  housepowerindate<-housepower[d,]
  globalactivepower<-as.numeric(housepowerindate[,3])
 ## par(mar=c(4,4,2,2))
  hist(globalactivepower, col = "red", xlab = "Global Active Power (kilowatts)")
  dev.copy(png, file = "plot1.png")
  dev.off()
}

Plot2<- function{
  housepowerconsumption<- read.table("household_power_consumption.txt", as.is=TRUE, header = TRUE )
  
  a<-strptime(housepower[,1],format="%d/%m/%Y")
  b<-a=="2007-02-01 00:00:00"
  c<-a=="2007-02-02 00:00:00"
  d<-b|c
  housepowerindate<-housepower[d,]
  globalactivepower<-as.numeric(housepowerindate[,3])
  datetime <- paste(housepowerindate[,1],housepowerindate[,2])
  t<-strptime(datetime, format= "%d/%m/%Y %H:%M:%S")
  plot(t,globalactivepower, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.copy(png, file = "plot2.png")
  dev.off()
}

Plot3<- function{
  housepowerconsumption<- read.table("household_power_consumption.txt", as.is=TRUE, header = TRUE )
  
  a<-strptime(housepower[,1],format="%d/%m/%Y")
  b<-a=="2007-02-01 00:00:00"
  c<-a=="2007-02-02 00:00:00"
  d<-b|c
  housepowerindate<-housepower[d,]
  globalactivepower<-housepowerindate[,3]
  datetime <- paste(housepowerindate[,1],housepowerindate[,2])
  t<-strptime(datetime, format= "%d/%m/%Y %H:%M:%S")
  plot(t,globalactivepower, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.copy(png, file = "plot3.png")
  dev.off()
}
Plot4<- function{
  housepowerconsumption<- read.table("household_power_consumption.txt", as.is=TRUE, header = TRUE )
  
  a<-strptime(housepower[,1],format="%d/%m/%Y")
  b<-a=="2007-02-01 00:00:00"
  c<-a=="2007-02-02 00:00:00"
  d<-b|c
  housepowerindate<-housepower[d,]
  globalactivepower<-housepowerindate[,3]
  datetime <- paste(housepowerindate[,1],housepowerindate[,2])
  t<-strptime(datetime, format= "%d/%m/%Y %H:%M:%S")
  
  par(mfrow=c(2,2))
  
  plot(t,globalactivepower, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
  plot(t,housepowerindate[,5],type="l",xlab="datetime",ylab = "Voltage")
  
  plot(t,housepowerindate[,7],type="n", xlab ="", ylab ="Energy sub metering")
  lines(t,housepowerindate[,7])
  lines(t,housepowerindate[,8],col="red")
  lines(t,housepowerindate[,9],col="blue")
  legend(x="topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1,col=c("black","red","blue"))
  plot(t,housepowerindate[,4],type="l",ylab="Global_reactive_power",xlab = "datetime")
  dev.copy(png, file = "plot4.png")
  dev.off()
  
}