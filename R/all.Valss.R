all.Valss<-function(){
     cat("\n")
     note_for_Valss_input()
     cat("\n")
     note_for_close_window()
     ### next line is for demo and testing only. -YJ
     ### ValSSpar<-data.frame(Parameter=c("age (yr)","INDI","CBZ","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),Value=c(78,1,0,400,12,10,150))
     ValSSpar<-data.frame(Parameter=c("age (yr)","INDI","CBZ","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),Value=c(0))
     ValSSpar<-edit(ValSSpar)    ### remarked this for testing. -YJ
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(ValSSpar);cat("\n\n")     
##     ValSSpar<-ycheck(ValSSpar)
     cat("\n")
     Val.ss(ValSSpar[7,2],ValSSpar[5,2],ValSSpar[6,2],ValSSpar[4,2],ValSSpar[1,2],ValSSpar[3,2],ValSSpar[2,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()
     note_for_Valss_output(ValSSpar[6,2])
     ### show(samplesStats("*"))
     cat("\n") 
     C1<-Valcpr(ValSSpar[4,2],ValSSpar[5,2],ValSSpar[6,2])    ### at time = 'ts'
     C2<-Valcpr(ValSSpar[4,2],ValSSpar[5,2],ValSSpar[5,2])    ### at time = 'tau'; the line is correct.   -YJ
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Cmss_pr (mg/L)","Ctss_pr (mg/L)"),Values=c(C1,C2))
     cat("\n\n");show(coutput);cat("\n")   
     ### Val.more(ValSSpar[4,2])   ### originally 'ValSSpar[4,2] = ka'  we will leave it as one of to be estimated parameters. -YJ
     Val.more()
}