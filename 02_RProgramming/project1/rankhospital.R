rankhospital <- function(state, Output, num = "best") {
 ## Read outcome data
 ## Check that state and outcome are valid
 ## Return hospital name in that state with the given rank
 ## 30-day death rate
        outcome[,11]<-as.numeric(outcome[,11])
        outcome[,17]<-as.numeric(outcome[,17])
        outcome[,23]<-as.numeric(outcome[,23])
        
        if (sum(Output==c("pneumonia","heart attack","heart failure"))==0){
                stop(gettextf("invalid outcome"))
        }
        if (sum(as.character(state)==outcome$State)==0){
                stop(gettextf("invalid state"))
        }  
        if (Output=="heart attack"){
                if (num=="best"){
        outp<-sort(outcome[(outcome[,11]==
                    min(outcome[(outcome$State==state),11]
                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if (num=="worst"){
        outp<-sort(outcome[(outcome[,11]==
                    max(outcome[(outcome$State==state),11]
                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state)+1)){
                        temp<-outcome[(outcome$State==state),]
                        temp$rank<-rank(temp[order(temp[,11], temp[,2], na.last=T,decreasing= F),11],na.last = TRUE,ties.method = "first")
                        temp[,2]<-temp[order(temp[,11], temp[,2], na.last=T,decreasing= F),2]
                        outp<-temp[temp$rank==num,2]
                } else{
                        outp<-NA
                }
                } else if (Output=="pneumonia"){
                if (num=="best"){
                        outp<-sort(outcome[(outcome[,23]==
                                                    min(outcome[(outcome$State==state),23]
                                                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if (num=="worst"){
                        outp<-sort(outcome[(outcome[,23]==
                                                    max(outcome[(outcome$State==state),23]
                                                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state)+1)){
                        temp<-outcome[(outcome$State==state),]
                        temp$rank<-rank(temp[order(temp[,23], temp[,2], na.last=T,decreasing= F),23],na.last = TRUE,ties.method = "first")
                        temp[,2]<-temp[order(temp[,23], temp[,2], na.last=T,decreasing= F),2]
                        outp<-temp[temp$rank==num,2]
                } else{
                        outp<-NA
                }
        } else if (Output=="heart failure"){
                if (num=="best"){
                        outp<-sort(outcome[(outcome[,17]==
                                                    min(outcome[(outcome$State==state),17]
                                                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if (num=="worst"){
                        outp<-sort(outcome[(outcome[,17]==
                                                    max(outcome[(outcome$State==state),17]
                                                        ,na.rm=T))&(outcome$State==state),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state)+1)){
                        temp<-outcome[(outcome$State==state),]
                        temp$rank<-rank(temp[order(temp[,17], temp[,2], na.last=T,decreasing= F),17],na.last = TRUE,ties.method = "first")
                        temp[,2]<-temp[order(temp[,17], temp[,2], na.last=T,decreasing= F),2]
                        outp<-temp[temp$rank==num,2]
                } else{
                        outp<-NA
                }
        }
        outp
        }