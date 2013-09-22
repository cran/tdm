all.ChiValss<-function(){
     cat("\n")
     note_for_ChiValss_input()
     cat("\n")
     note_for_close_window()
     ### ChiValSSpar<-data.frame(parameter=c("TBW (Kg)","CBZ","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),value=c(0))
     ChiValSSpar<-data.frame(parameter=c("TBW (Kg)","CBZ","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),value=c(12,1,200,12,10,150))  ## for testing -YJ
     ChiValSSpar<-edit(ChiValSSpar)    ### remarked this for testing. -YJ
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(ChiValSSpar);cat("\n\n")
##     ChiValSSpar<-ycheck(ChiValSSpar)
     ChiVal.ss(ChiValSSpar[6,2],ChiValSSpar[4,2],ChiValSSpar[5,2],ChiValSSpar[3,2],ChiValSSpar[1,2],ChiValSSpar[2,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_ChiValss_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-ChiValcpr(ChiValSSpar[3,2],ChiValSSpar[4,2],ChiValSSpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Cmss_pr (mg/L)")
     output1<-coutput
     show(coutput)
     C<-ChiValcpr(ChiValSSpar[3,2],ChiValSSpar[4,2],ChiValSSpar[4,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     output2<-coutput
     show(coutput)    
     cat("\n")
     ChiVal.more()
}