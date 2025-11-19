 pollutantmean2 <- function(directory, pollutant, id=1:332) {
        file_list <- list.files(file.path(getwd(),directory))
        me<-matrix(rep(0,length(id)),1,length(id))

        for (i in 1:length(id)){
                x<-read.csv(file.path(getwd(),directory,file_list[i]))
                xcol<-x[pollutant]
                me[i]<-mean(xcol[!is.na(xcol)])
#                 me[i]<-mean(xcol[,],na.rm=T)
        }
        mean(me[,])
 }     