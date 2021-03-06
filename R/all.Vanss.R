all.Vanss<-function(){
     cat("\n")
     note_for_Vanss_input()
     cat("\n")
     note_for_close_window()
     VanSSpar<-data.frame(parameter=c("Gender","age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","tin (hr)","ts (hr)","c (mg/L)"),value=c(0))
     VanSSpar<-edit(VanSSpar)
     cat("\n Input data are as follows:\n")
     cat(" ----------------------------\n")
     show(VanSSpar);cat("\n\n")
##     VanSSpar<-ycheck(VanSSpar)
     cat("\n")
     Van.ss(VanSSpar[9,2],VanSSpar[6,2],VanSSpar[8,2],VanSSpar[7,2],VanSSpar[5,2],VanSSpar[3,2],VanSSpar[4,2],VanSSpar[1,2],VanSSpar[2,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Vanss_output()
     ### show(samplesStats("*"))
     cat("\n")
     C1<-infcpr(VanSSpar[5,2],VanSSpar[7,2],VanSSpar[6,2],VanSSpar[8,2]+VanSSpar[7,2])
     ### read from saved .cvs -YJ
     X <- read.table("params.csv",header=FALSE)
     cl <- X[1,2]
     v  <- X[2,2]
     ###
     C2<-infcpr(VanSSpar[5,2],VanSSpar[7,2],VanSSpar[6,2],VanSSpar[6,2])/(exp(-(cl/v)*(VanSSpar[6,2]-(VanSSpar[7,2]+1))))
     C3<-infcpr(VanSSpar[5,2],VanSSpar[7,2],VanSSpar[6,2],VanSSpar[6,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cmss_pr (mg/L)","Cpss_pr (mg/L)","Ctss_pr (mg/L)"),Values=c(C1,C2,C3))
     cat("\n\n");show(coutput);cat("\n")   
     Van.more()
}