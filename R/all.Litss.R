all.Litss<-function(){
     cat("\n")
     note_for_Lit_input()  
     cat("\n")
     note_for_close_window()
     ### LitSSpar<-data.frame(parameter=c("age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","Css (mEq/L)"),value=c(0))
     LitSSpar<-data.frame(parameter=c("age (yr)","bw (kg)","Scr (mg/dL)","D (mg)","tau (hr)","Css (mEq/L)"),value=c(65,67,2.1,325,12,2.1))
     LitSSpar<-edit(LitSSpar)            ### remarked this line for model testing.  --YJ
     f<-ifelse(LitSSpar[1,2]>50,1.,0.)   ### 'LitSSpar[1,2]' = age; users save one more input in this case. great. -YJ
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(LitSSpar);cat("\n\n")
##     LitSSpar<-ycheck(LitSSpar)
     cat("\n")
     Lit.ss(LitSSpar[6,2],LitSSpar[5,2],LitSSpar[1,2],LitSSpar[4,2],LitSSpar[3,2],LitSSpar[2,2],f)  ### 'f' still req.
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Lit_output()
     ### show(samplesStats("*"))
     cat("\n") 
     C<-Litcpr(LitSSpar[4,2],LitSSpar[5,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(C)
     colnames(coutput)<-list("Cmss_pr (mEq/L)")
     show(coutput)
     cat("\n")   
     Lit.more()
}