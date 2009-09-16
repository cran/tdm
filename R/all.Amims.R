all.Amims<-function(){
     cat("\n")
     note_for_Amiss_input()
     cat("\n")
     note_for_close_window()
     AmiMSpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),bw=c(0),Ht=c(0),Scr=c(0),D=c(0),tau=c(0),tin=c(0),ts=c(0),c=c(0))    
     AmiMSpar<-edit(AmiMSpar)
##     AmiMSpar<-ymscheck(AmiMSpar)
     cat("\n")
     Ami.ms(length(AmiMSpar$subject),AmiMSpar$c,AmiMSpar$tau,AmiMSpar$ts,AmiMSpar$tin,AmiMSpar$D,AmiMSpar$bw,AmiMSpar$Ht,AmiMSpar$Scr,AmiMSpar$Gender,AmiMSpar$age)  # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     note_for_plot()
     convergence_plots_con()
     note_for_Amiss_output()
     show(samplesStats("*"))
     cat("\n")
     C<-infcpr(AmiMSpar$D,AmiMSpar$tin,AmiMSpar$tau,AmiMSpar$ts+AmiMSpar$tin)  
     sim<-matrix(C[ ,1])                                                         # 取表格[,1]之答案
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cmss_pr (mg/L)")
     output1<-coutput
     show(coutput) 
     C<-infcpr(AmiMSpar$D,AmiMSpar$tin,AmiMSpar$tau,AmiMSpar$tin)  
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     output2<-coutput
     show(coutput)  
     C<-infcpr(AmiMSpar$D,AmiMSpar$tin,AmiMSpar$tau,AmiMSpar$tau)  
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     output3<-coutput
     show(coutput)                            
     cat("\n")
     cal.again()
}