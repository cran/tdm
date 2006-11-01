Phelimit<-function(a,b){
R=a*24/b
     if (samplesStats("Vmax")[1,1]>R){ 
     C<-(a*24/b)*(samplesStats("Km"))/((samplesStats("Vmax"))-a*24/b) 
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css(mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")                          
       file.menu <- c("Yes",
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Phe.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         } 
         }
  if (!samplesStats("Vmax")[1,1]>R){
     cat("\n\n")
     cat("*******************************************************\n")
     cat("      ATTENTION :                                      \n")
     cat("      Desired dose must be less than Vmax.             \n")
     cat("*******************************************************\n")
     show(samplesStats("Vmax"))
     cat("\n\n")
     cat("          Pressing Enter to continue..                 \n")
     readline()
     Pheclpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Pheclpar<-edit(Pheclpar)
     Pheclpar<-check(Pheclpar)
     Phelimit(Pheclpar[1,2],Pheclpar[2,2])
}
}


