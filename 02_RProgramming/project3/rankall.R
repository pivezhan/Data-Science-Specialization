rankall <- function(Output, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        outcome[,11]<-as.numeric(outcome[,11])
        outcome[,17]<-as.numeric(outcome[,17])
        outcome[,23]<-as.numeric(outcome[,23])
        state <- sort(c( "AK","AL", "AZ", "AR", "CA", "CO", "CT",
                          "DE","DC", "FL", "GA", "HI", "ID", "IL", "IN",
                          "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                          "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                          "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                          "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                          "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
        
        
        if (sum(Output==c("pneumonia","heart attack","heart failure"))==0){
                stop(gettextf("invalid outcome"))
        }
        hospital<-rep(0,length(state))
        
        for (i in 1:length(state)){ 
        if (Output=="heart attack"){
                if (num=="best"){
                hospital[i]<-sort(outcome[(outcome[,11]==
                                    min(outcome[(outcome$State==state[i]),11]
                                        ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if (num=="worst"){
                hospital[i]<-sort(outcome[(outcome[,11]==
                                    max(outcome[(outcome$State==state[i]),11]
                                        ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state[i])+1)){
                temp<-outcome[(outcome$State==state[i]),]
                temp$rank<-rank(temp[order(temp[,11], temp[,2], na.last=T,decreasing= F),11],
                                na.last = TRUE,ties.method = "first")
                temp[,2]<-temp[order(temp[,11], temp[,2], na.last=T,decreasing= F),2]
                hospital[i]<-temp[temp$rank==num,2]
                } else{
                hospital[i]<-NA
                }
        } else if (Output=="pneumonia"){
                if (num=="best"){
                hospital[i]<-sort(outcome[(outcome[,23]==
                            min(outcome[(outcome$State==state[i]),23]
                                ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if (num=="worst"){
                hospital[i]<-sort(outcome[(outcome[,23]==
                    max(outcome[(outcome$State==state[i]),23]
                                ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state[i])+1)){
                temp<-outcome[(outcome$State==state[i]),]
                temp$rank<-rank(temp[order(temp[,23], temp[,2], na.last=T,decreasing= F),23],
                                na.last = TRUE,ties.method = "first")
                temp[,2]<-temp[order(temp[,23], temp[,2], na.last=T,decreasing= F),2]
                hospital[i]<-temp[temp$rank==num,2]
                } else{
                hospital[i]<-NA
                }
        } else if (Output=="heart failure"){
                if (num=="best"){
                hospital[i]<-sort(outcome[(outcome[,17]==
                                    min(outcome[(outcome$State==state[i]),17]
                                        ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if (num=="worst"){
                hospital[i]<-sort(outcome[(outcome[,17]==
                                    max(outcome[(outcome$State==state[i]),17]
                                        ,na.rm=T))&(outcome$State==state[i]),2],decreasing= F)        
                } else if(num<(sum(outcome$State==state[i])+1)){
                temp<-outcome[(outcome$State==state[i]),]
                temp$rank<-rank(temp[order(temp[,17], temp[,2], na.last=T,decreasing= F),17],
                                na.last = TRUE,ties.method = "first")
                temp[,2]<-temp[order(temp[,17], temp[,2], na.last=T,decreasing= F),2]
                hospital[i]<-temp[temp$rank==num,2]
                } else{
                hospital[i]<-NA
                }
        }
        }
        data.frame(hospital,state)
}