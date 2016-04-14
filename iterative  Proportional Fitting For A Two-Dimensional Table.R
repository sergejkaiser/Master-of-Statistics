#2x2 raw numbers and margins in  third olumn, third row
bush.kerry<-matrix(c(220,291,530,226,263,470,520,480,1000),ncol=3,nrow=3,byrow=T)
#iterative  Proportional Fitting For A Two-Dimensional Table
 ipf<- function(ma) {
   #ipf for 2x2 tables
   row.numbers<-nrow(ma)
   row.numbers2<-(row.numbers-1)
   col.numbers<-ncol(ma)
   col.numbers2<-(row.numbers-1)
   ma.sub<-ma[-row.numbers,-col.numbers]
   ma.sub.adj<-matrix(c(data=0,row=row.numbers2,col=col.numbers2))
 #iteration  
   count<-1
   succes<-0
   while(succes==0) {  
   if (count==1) { 
     
  for (i  in 1:nrow(ma.sub.adj)) { 
     ma.sub.adj[i,]<-(ma.sub[i,]/rowSums(ma.sub[i,]))*(ma[-row.numbers,col.numbers])
   }
     print(ma.sub.adj)
  for (j in 1:ncol(ma.sub.adj)) { 
        ma.sub.adj[,j]<-(ma.sub[,j]/rowSums(ma.sub[,j]))*ma[row.numbers ,-col.numbers]
  }
     print(ma.sub.adj)
   rowes<- rowSums(ma.sub.adj)-ma[row.numbers,-(col.numbers)]< 0.1 
   print(rowes)
   #vector operation each vector elemaent mainus the other vector elemaent
   coles <- colSums(ma.sub.adj)-ma[-row.numbers,(col.numbers)] < 0.1 
   print(coles)
     success <- ifelse( length(rowes[rowes==TRUE])+length(coles[coles==TRUE]) == 4, 1, 0)
   #success <- ifelse( length(rowes[rowes==TRUE]) == 2, TRUE, FALSE)  
   count<-count+1
   }
  #if it didnt converge in fist attemapt, than iterate
   else { 
     #adjustmaent cycle first row than columan
     
     for (i  in 1:nrow(ma.sub.adj)) { 
       ma.sub.adj[i,]<-(ma.sub.adj[i,]/rowSums(ma.sub.adj[i,]))*(ma[-row.numbers,(col.numbers)])
     }
     
     for (j in 1:ncol(ma.sub.adj)) { 
       ma.sub.adj[,j]<-(ma.sub.adj[,j]/rowSums(ma.sub.adj[,j]))*ma[ ,row.numbers]
     }
     rowes<- rowSums(ma.sub.adj)-ma[row.numbers,-(col.numbers)]< 0.1 
     #vector operation each vector element minus the other vector element
     coles <- colSums(ma.sub.adj)-ma[-row.numbers,(col.numbers)] < 0.1 
     
     success <- ifelse( length(rowes[rowes==TRUE])+length(coles[coles==TRUE]) == 4, 1, 0)
     #success <- ifelse( length(rowes[rowes==TRUE]) == 2, TRUE, FALSE)  
   }
   }
   ma.sub.adj
}
