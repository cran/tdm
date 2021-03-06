all.Enfsm<-function(){
     cat("\n")
     note_for_Enfsm_input()
     cat("\n")
     note_for_close_window()
     EnfSMMpar<-data.frame(parameter=c("Gender","bw (kg)","D (mg)","tau (hr)"),
                           value=c(0,78,90,12))
     EnfSMMpar<-edit(EnfSMMpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(EnfSMMpar);cat("\n\n")
##     EnfSMMpar<-zcheck(EnfSMMpar)
     note_for_Enfsm_conc_input()
     cat("\n")
     note_for_close_window()
     EnfSMpar<-data.frame(ts=c(6, 10),conc=c(1.65, 1.12))
     EnfSMpar<-edit(EnfSMpar)
     cat("\n Input conc. data are as follows:\n")
     cat(" --------------------------------\n")
     show(EnfSMpar);cat("\n\n")
##     EnfSMpar<-mscheck(EnfSMpar)
     cat("\n")
     Enf.sm(length(EnfSMpar$ts),EnfSMpar$conc,EnfSMpar$ts,EnfSMMpar[4,2],EnfSMMpar[3,2],EnfSMMpar[2,2],EnfSMMpar[1,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Enfsm_output()
     ### show(samplesStats("*"))
     ### show obs. and calc. conc. here  -YJ
     cat("\n\n")
     for(i in 1:length(EnfSMpar$ts)){
         Cx<-enfcpr(EnfSMMpar[3,2],EnfSMMpar[4,2],EnfSMpar$ts[i]) 
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(EnfSMpar$conc[i],Cx))
         cat("--- Enfuvirtide (T20) plasma conc. (mg/L) at Time =",EnfSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     cat("\n") 
     C<-enfcpr(EnfSMMpar[3,2],EnfSMMpar[4,2],EnfSMMpar[4,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("\n") 
     Enf.more()
}