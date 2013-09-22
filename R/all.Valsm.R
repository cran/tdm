all.Valsm<-function(){
     cat("\n")
     note_for_Valsm_input()
     cat("\n")
     note_for_close_window()
     ValSMMpar<-data.frame(parameter=c("age (yr)","INDI","CBZ","D (mg)","tau (hr)"),value=c(78,1,0,400,12))  ### for testing  -YJ
     ### ValSMMpar<-data.frame(parameter=c("age (yr)","INDI","CBZ","D (mg)","tau (hr)"),value=c(0))
     ValSMMpar<-edit(ValSMMpar)   ### remarked this for testing. -YJ
##     ValSMMpar<-ycheck(ValSMMpar)
     cat("\n Input data are as follows:\n")
     cat(" ----------------------------\n")
     show(ValSMMpar);cat("\n\n") 
     cat("\n")
     note_for_Valsm_conc_input()
     cat("\n")
     note_for_close_window()
     ValSMpar<-data.frame(ts=c(6,10),conc=c(176,150)) ### for testing  -YJ
     ValSMpar<-edit(ValSMpar)    ### remarked this for testing. -YJ
     ### ValSMpar<-mscheck(ValSMpar) ### remarked this for testing. Hmmm... this may cause errors!  -YJ
     cat("\n Input conc. are as follows:\n")
     cat(" -----------------------------\n")
     show(ValSMpar);cat("\n\n") 
     cat("\n")
     Val.sm(length(ValSMpar$ts),ValSMpar$conc,ValSMpar$ts,ValSMMpar[5,2],ValSMMpar[4,2],ValSMMpar[1,2],ValSMMpar[3,2],ValSMMpar[2,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Valsm_output()
     ### show(samplesStats("*"))
     ### show obs. & calc. conc. here  -YJ
     cat("\n\n")
     for(i in 1:length(ValSMpar$ts)){
         Cx<-Valcpr(ValSMMpar[4,2],ValSMMpar[5,2],ValSMpar$ts[i])
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(ValSMpar$conc[i],Cx))
         cat("--- Valproate plasma conc. (mg/L) at Time =",ValSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     cat("\n") 
     C<-Valcpr(ValSMMpar[4,2],ValSMMpar[5,2],ValSMMpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("\n") 
     Val.more()
}