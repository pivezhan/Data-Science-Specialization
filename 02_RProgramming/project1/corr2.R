corr2 <- function(directory, threshold=0) {
        id<-332
        CompleteCases<-complete(directory, 1:id)
        file_list <- list.files(file.path(getwd(),directory))
        cor2<-matrix(rep(0,length(id)),1,length(id))

        for (i in 1:id){
                        if (CompleteCases[i,2]>threshold){
                                x<-read.csv(file.path(getwd(),directory,file_list[i]))
                                x1<-x[,2]
                                x2<-x[,3]
                                cor2[i]<-cor(x1[!(is.na(x1))&!(is.na(x2))],x2[!(is.na(x1))&!(is.na(x2))])
                        }else{
                                cor2[i]<-NA
                        }
                                }
        cor2[!(is.na(cor2))] 
}