# Avoid Dose greater than Vmax when performing dosage adjustment for phenytoin
Phelimit<-function(a,b){
     X <- read.table("params.csv",header=FALSE)
     km   <- X[1,2]
     vmax <- X[2,2]
                                             
     R=a*24/b
     ## if (samplesStats("Vmax")[1,1]>R){         ### for BUGS; if Vamx > Dose, tdm would continue to adjust does and ask user does he want to dose adjustment again?
     if (vmax>R ||vmax==R){                       ### for JAGS; if Vamx > Dose, tdm would continue to adjust does and ask user does he want to dose adjustment again?
     ### C<-(a*24/b)*(samplesStats("Km"))/((samplesStats("Vmax"))-a*24/b) 
     C<-(a*24/b)*km/(vmax-a*24/b) 
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","Tau (hr)"," -> Css_trough (mg/L)"),Values=c(a, b, C))
     cat("\n");show(coutput);cat("\n")            
     Phe.more()   
   }            
  if (vmax<R){                                       # if Vamx < Dose, we would give user a caution and ask user to edit desired Does agian
     cat("\n\n")
     cat("*******************************************************\n")
     cat("      ATTENTION :                                      \n")
     cat("      Desired dose must be less than Vmax (=",vmax,")  \n")
     cat("*******************************************************\n")
     cat("\n\n")
     cat("          Pressing Enter to continue..                 \n")
     readline()
     Pheclpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(300,24))
     Pheclpar<-edit(Pheclpar)
     ### Pheclpar<-check(Pheclpar)
     show(Pheclpar);cat("\n\n")
     Phelimit(Pheclpar[1,2],Pheclpar[2,2])
}
}
