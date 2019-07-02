#load necessary packages
library(ade4)
library(cluster)
library(tools)

#read in file/dir
setwd("/home/hirschc1/lixx5447/projects/nil/github/mantel")
filenames<- list.files(path = getwd(),pattern = "csv", full.names =F)

#define function
dist<- function(myfile){	
	data<- read.csv(file=myfile,as.is=T, na.strings=".", header=T)	
	data.use=data[c(1:17),c(7:18)]	
	mydist<- daisy(data.use, metric="euclidean", stand=TRUE)#daisy can deal with the missing data
	print(myfile)	
	mydist<- as.matrix(mydist)#just for the purpose of export the distance matrix
    write.csv(mydist, paste0("dist_", myfile), row.names=F)  
}

#execute function
for (f in filenames){
	dist(f)
}
# mantel function
filenames2<- list.files(path = getwd(),pattern = "dist", full.names =F)
mymantel <- function(file1, file2){        
        data1<- read.csv(file=file1, as.is=T, na.strings=".", header=T)
        data2<- read.csv(file=file2, as.is=T, na.strings=".", header=T) 
        mymantel<- mantel.rtest(as.dist(data1), as.dist(data2),nrepet = 999)# convert read in matrix to the required dist format        
        #print(mymantel)
}

result <- NULL #set the result variable
for (file1 in filenames2) {
	for (file2 in filenames2) {
        result<- rbind(result, mymantel(file1, file2))#combine the result of mymantel
        capture.output(result,file="/home/hirschc1/lixx5447/projects/nil/github/mantel/mantel999.txt")
        #write.csv(result,file="mantel.csv")
        print(file1)
        print(file2)
    }
}


