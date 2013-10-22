all.Litcitss<-function(){
     cat("\n")
     note_for_Litcit_input()
     cat("\n")
     note_for_close_window()
     ### LitcitSSpar<-data.frame(parameter=c("age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","Css (mEq/L)"),value=c(0))
     LitcitSSpar<-data.frame(parameter=c("age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","Css (mEq/L)"),value=c(65,67,2.1,650,12,2.1))
     LitcitSSpar<-edit(LitcitSSpar)            ### remarked this line for model testing.  --YJ
     f<-ifelse(LitcitSSpar[1,2]>50,1.,0.)   ### 'LitSSpar[1,2]' = age; users save one more input in this case. great. -YJ
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(LitcitSSpar);cat("\n\n")
##     LitcitSSpar<-ycheck(LitcitSSpar)
     cat("\n")
     Litcit.ss(LitcitSSpar[6,2],LitcitSSpar[5,2],LitcitSSpar[1,2],LitcitSSpar[4,2],LitcitSSpar[3,2],LitcitSSpar[2,2],f)  ### 'f' still req.
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Litcit_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-Litcitcpr(LitcitSSpar[4,2],LitcitSSpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Cmss_pr (mEq/L)")
     show(coutput)
     cat("\n")
     Litcit.more()
}