all.Cycsm<-function(){
     cat("\n")
     note_for_Cycsm_input()
     cat("\n")
     note_for_close_window()
     ### CycSMMpar<-data.frame(parameter=c("bw (kg)","PTD (day)","Dia","D (mg)","tau (hr)"),value=c(0))
     CycSMMpar<-data.frame(parameter=c("bw (kg)","PTD (day)","Dia","D (mg)","tau (hr)"),value=c(78,30,0,600,12))   ### for model testing.
     CycSMMpar<-edit(CycSMMpar)   ### for model testing.
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(CycSMMpar);cat("\n\n")
##     CycSMMpar<-zcheck(CycSMMpar)
     cat("\n")
     note_for_Cycsm_conc_input()
     cat("\n")
     note_for_close_window()
     ### CycSMpar<-data.frame(ts=c(0),conc=c(0))
     CycSMpar<-data.frame(ts=c(2,10),conc=c(800,700))    ### for model testing.
     CycSMpar<-edit(CycSMpar)    ### for model testing.
     cat("\n")
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(CycSMpar);cat("\n\n")
##     CycSMpar<-mscheck(CycSMpar)
     cat("\n")
     Cyc.sm(length(CycSMpar$ts),CycSMpar$conc,CycSMpar$ts,CycSMMpar[2,2],CycSMMpar[5,2],CycSMMpar[4,2],CycSMMpar[1,2],CycSMMpar[3,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Cycsm_output()
     ### show(samplesStats("*"))
     ### show obs. and calc. conc. here  -YJ
     cat("\n\n")
     for(i in 1:length(CycSMpar$ts)){
         Cx<-Cyccpr(CycSMMpar[2,2],CycSMMpar[1,2],CycSMMpar[4,2],CycSMMpar[5,2],CycSMpar$ts[i])
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(CycSMpar$conc[i],Cx))
         cat("--- CSA plasma conc. (mcg/L) at Time =",CycSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     ### here we can also calculate C(2) =?  --YJ
     C2<-Cyccpr(CycSMMpar[2,2],CycSMMpar[1,2],CycSMMpar[4,2],CycSMMpar[5,2],2)  ### set 'ts = 2' to calc. C(2)ss here
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C2)
     colnames(coutput)<-list("C(2)ss_pr (mcg/L)")
     output1<-coutput
     show(coutput);cat("\n")
     C3<-Cyccpr(CycSMMpar[2,2],CycSMMpar[1,2],CycSMMpar[4,2],CycSMMpar[5,2],CycSMMpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Ctss_pr (or C(0)ss_pr) (mcg/L)")
     coutput<-data.frame(Parameters=c("C(2)ss_pr (mcg/L)","Ctss_pr (or C(0)ss_pr) (mcg/L)"),Values=c(C2,C3))
     cat("\n\n");show(coutput);cat("\n")
     ### Cyc.more(CycSSpar[2,2],CycSSpar[1,2
     Cyc.more(30,CycSMMpar[1,2])  ### here 30 = PTD; 'CycSMMpar[1,2]' = BW. -YJ
}