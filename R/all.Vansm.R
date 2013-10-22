all.Vansm<-function(){
     cat("\n")
     note_for_Vansm_input()
     cat("\n")
     note_for_close_window()
     ### VanSMMpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)"),value=c(0))
     VanSMMpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)"),
                               value=c(1,67,82,1.3,500,12,1.0))
     VanSMMpar<-edit(VanSMMpar)
     cat("\n Input data are as follows:\n")
     cat(" ----------------------------\n")
     show(VanSMMpar);cat("\n\n")
##     VanSMMpar<-ycheck(VanSMMpar)
     cat("\n")
     note_for_Vansm_conc_input()
     cat("\n")
     note_for_close_window()
     VanSMpar<-data.frame(ts=c(6,10),conc=c(21.1,18.7))
     VanSMpar<-edit(VanSMpar)
     ### VanSMpar<-mscheck(VanSMpar)
     cat("\n Input Conc are as follows:\n")
     cat(" ----------------------------\n")
     show(VanSMpar);cat("\n\n")
     cat("\n")
     Van.sm(length(VanSMpar$ts),VanSMpar$conc,VanSMpar$ts,VanSMMpar[6,2],VanSMMpar[7,2],VanSMMpar[5,2],VanSMMpar[3,2],VanSMMpar[4,2],VanSMMpar[1,2],VanSMMpar[2,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Vansm_output()
     ### show(samplesStats("*"))
     ### read from saved .cvs -YJ
     X <- read.table("params.csv",header=FALSE)
     cl <- X[1,2]
     v  <- X[2,2]
     ###     
     ### show obs. and calc. conc. here  -YJ
     cat("\n\n")
     C.peak<-infcpr(VanSMMpar[5,2],VanSMMpar[7,2],VanSMMpar[6,2],VanSMMpar[6,2])/(exp(-(cl/v)*(VanSMMpar[6,2]-(VanSMMpar[7,2]+1))))  ### the C_peack  -YJ
     for(i in 1:length(VanSMpar$ts)){
         Cx<-C.peak*exp(-cl/v*(VanSMpar$ts[i]))
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(VanSMpar$conc[i],Cx))
         cat("--- Vanco plasma conc. (mg/L) at Time =",VanSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }
     cat("\n")
     ### C<-infcpr(VanSMMpar[5,2],VanSMMpar[7,2],VanSMMpar[6,2],VanSMMpar[6,2])/(exp(-(cl/v)*(VanSMMpar[6,2]-(VanSMMpar[7,2]+1))))  ### the C_peack  -YJ
     C2<-C.peak                           ### don't need to calc. again here -YJ
     C3<-infcpr(VanSMMpar[5,2],VanSMMpar[7,2],VanSMMpar[6,2],VanSMMpar[6,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cpss_pr (mg/L)","Ctss_pr (mg/L)"),Values=c(C2,C3))
     cat("\n\n");show(coutput);cat("\n")
     Van.more()
}