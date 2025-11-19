complete<-function(directory,id=1:332){
        file_list <- list.files(file.path(getwd(),directory))
        nobs<-matrix(rep(0,length(id)),1,length(id))
        maxid<-max(id)
        c<-1
        for(letters in id) {
                for (i in 1:id[c]){
                        if (i==id[c]){
                                x<-read.csv(file.path(getwd(),directory,file_list[i]))
                                nobs[c]<-length(completed.cases(x))
#                                 x1<-x[,2]
#                                 x2<-x[,3]
#                                 ob1<-length(x1[!is.na(x1)])
#                                 ob2<-length(x2[!is.na(x2)])
#                                 nobs[c]<-min(ob1,ob2)
                                c<-c+1
                        }
                }
        }
        data.frame(id,nobs=nobs[,])
}