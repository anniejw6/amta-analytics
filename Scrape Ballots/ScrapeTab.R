library(xlsx)
setwd("~/Dropbox/Tabulation Sub-Committee/Scrape_Tab_Summaries")


#########################
###Scrape a Single Tab###
#########################
#data <- read.xlsx("2014STLORCS.xls",1, stringsAsFactors=FALSE)
data <- read.xlsx("2013STLORCS.xls",1, stringsAsFactors=FALSE)

#Remove Unusued Rows/Columns
dataCUT <- data[6:(which(data[,1]=="Tabulation Room Notes")-1),c(1,4,6,7,9,10,12,13,15,16,18,20)]
#Remove header that starts each additional page
headerRows <- which(dataCUT[,1]=="Team/School")
dataCUT <- dataCUT[-headerRows,]
#Rename Columns
names(dataCUT) <- c("team","r1.1","r1.2","r2.1","r2.2","r3.1","r3.2","r4.1","r4.2","cs","ocs","pd")

noTeams <- (dim(dataCUT)[1])/3
scrapedData <- data.frame(list(teamNumber=rep(NA,length.out=noTeams),teamName=NA,r1side=NA,r1opponent=NA,r1ballot1=NA,r1ballot2=NA,r2side=NA,r2opponent=NA,r2ballot1=NA,r2ballot2=NA,r3side=NA,r3opponent=NA,r3ballot1=NA,r3ballot2=NA,r4side=NA,r4opponent=NA,r4ballot1=NA,r4ballot2=NA,totalWins=NA,totalLosses=NA,totalTies=NA,cs=NA,ocs=NA,pd=NA))
t <- 1
i <- 1
for (j in 1:((dim(dataCUT)[1]-1)/3) ) {
	scrapedData$teamNumber[t] <- dataCUT$team[i]
	scrapedData$teamName[t] <- dataCUT$team[i+1]
	scrapedData$r1side[t] <- dataCUT$r1.1[i]
	scrapedData$r1opponent[t] <- dataCUT$r1.2[i]
	scrapedData$r1ballot1[t] <- dataCUT$r1.1[i+2]
	scrapedData$r1ballot2[t] <- dataCUT$r1.2[i+2]
	scrapedData$r2side[t] <- dataCUT$r2.1[i]
	scrapedData$r2opponent[t] <- dataCUT$r2.2[i]
	scrapedData$r2ballot1[t] <- dataCUT$r2.1[i+2]
	scrapedData$r2ballot2[t] <- dataCUT$r2.2[i+2]
	scrapedData$r3side[t] <- dataCUT$r3.1[i]
	scrapedData$r3opponent[t] <- dataCUT$r3.2[i]
	scrapedData$r3ballot1[t] <- dataCUT$r3.1[i+2]
	scrapedData$r3ballot2[t] <- dataCUT$r3.2[i+2]
	scrapedData$r4side[t] <- dataCUT$r4.1[i]
	scrapedData$r4opponent[t] <- dataCUT$r4.2[i]
	scrapedData$r4ballot1[t] <- dataCUT$r4.1[i+2]
	scrapedData$r4ballot2[t] <- dataCUT$r4.2[i+2]
	scrapedData$totalWins[t] <- dataCUT$cs[i]
	scrapedData$totalLosses[t] <- dataCUT$ocs[i]
	scrapedData$totalTies[t] <- dataCUT$pd[i]
	scrapedData$cs[t] <- dataCUT$cs[i+2]
	scrapedData$ocs[t] <- dataCUT$ocs[i+2]
	scrapedData$pd[t] <- dataCUT$pd[i+2]	
	t <- t+1
	i <- i+3
}


#write.csv(scrapedData,"2014_STL_ORCS_scraped.csv")
write.csv(scrapedData,"2013_STL_ORCS_scraped.csv")


#########################
####Scrape all Excel ####
####Files in Directory###
#########################

filelist <- dir(pattern=".xls")
savefilelist <- gsub(".xls",".csv",filelist)
for (k in 1:length(filelist)){
	data <- read.xlsx(filelist[k],1, stringsAsFactors=FALSE)
	dataCUT <- data[6:(which(data[,1]=="Tabulation Room Notes")-1),c(1,4,6,7,9,10,12,13,15,16,18,20)]

	#Remove header that starts each additional page
	headerRows <- which(dataCUT[,1]=="Team/School")
	dataCUT <- dataCUT[-headerRows,]

	#Rename Columns
	names(dataCUT) <- c("team","r1.1","r1.2","r2.1","r2.2","r3.1","r3.2","r4.1","r4.2","cs","ocs","pd")

	noTeams <- (dim(dataCUT)[1])/3
	scrapedData <- 	data.frame(list(teamNumber=rep(NA,length.out=noTeams),teamName=NA,r1side=NA,r1opponent=NA,r1ballot1=NA,r1ballot2=NA,r2side=NA,r2opponent=NA,r2ballot1=NA,r2ballot2=NA,r3side=NA,r3opponent=NA,r3ballot1=NA,r3ballot2=NA,r4side=NA,r4opponent=NA,r4ballot1=NA,r4ballot2=NA,totalWins=NA,totalLosses=NA,totalTies=NA,cs=NA,ocs=NA,pd=NA))
	t <- 1
	i <- 1
	for (j in 1:((dim(dataCUT)[1]-1)/3) ) {
		scrapedData$teamNumber[t] <- dataCUT$team[i]
		scrapedData$teamName[t] <- dataCUT$team[i+1]
		scrapedData$r1side[t] <- dataCUT$r1.1[i]
		scrapedData$r1opponent[t] <- dataCUT$r1.2[i]
		scrapedData$r1ballot1[t] <- dataCUT$r1.1[i+2]
		scrapedData$r1ballot2[t] <- dataCUT$r1.2[i+2]
		scrapedData$r2side[t] <- dataCUT$r2.1[i]
		scrapedData$r2opponent[t] <- dataCUT$r2.2[i]
		scrapedData$r2ballot1[t] <- dataCUT$r2.1[i+2]
		scrapedData$r2ballot2[t] <- dataCUT$r2.2[i+2]
		scrapedData$r3side[t] <- dataCUT$r3.1[i]
		scrapedData$r3opponent[t] <- dataCUT$r3.2[i]
		scrapedData$r3ballot1[t] <- dataCUT$r3.1[i+2]
		scrapedData$r3ballot2[t] <- dataCUT$r3.2[i+2]
		scrapedData$r4side[t] <- dataCUT$r4.1[i]
		scrapedData$r4opponent[t] <- dataCUT$r4.2[i]
		scrapedData$r4ballot1[t] <- dataCUT$r4.1[i+2]
		scrapedData$r4ballot2[t] <- dataCUT$r4.2[i+2]
		scrapedData$totalWins[t] <- dataCUT$cs[i]
		scrapedData$totalLosses[t] <- dataCUT$ocs[i]
		scrapedData$totalTies[t] <- dataCUT$pd[i]
		scrapedData$cs[t] <- dataCUT$cs[i+2]
		scrapedData$ocs[t] <- dataCUT$ocs[i+2]
		scrapedData$pd[t] <- dataCUT$pd[i+2]	
		t <- t+1
		i <- i+3
	}
	write.csv(scrapedData,savefilelist[k])	
	print(filelist[k])	
}