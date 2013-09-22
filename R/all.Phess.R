all.Phess<-function(){
     cat("\n")
     note_for_Phe_input()
     cat("\n")
     note_for_close_window()
     PheSSpar<-data.frame(parameter=c("BW (kg)","Dose (mg)","tau (hr)","Conc. (mg/L)"),value=c(78,300,24,26.1))  ### for model testing -YJ
     ### PheSSpar<-data.frame(parameter=c("BW (kg)","Dose (mg)","tau (hr)","Conc. (mg/L)"),value=c(0))
     PheSSpar<-edit(PheSSpar)
##     PheSSpar<-zcheck(PheSSpar)
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(PheSSpar);cat("\n\n") 
     cat("\n")
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     ### convergence_plots_sep()                      ### for BUGS only. -YJ
     note_for_Phe_output()    
     ### show(samplesStats("*"))    ### for BRugs  -YJ
     ### cat("\n") 
     ### C<-Phecpr(PheSSpar[2,2],PheSSpar[3,2])
     ### Final<-(c(matrix(C[1 ,1]), PheSSpar[4,2]))
     ### coutput<-data.frame(Final)
     ### row.names(coutput)<-list("C(ss_calc)","C(ss_obs)")
     ### show(coutput)
     cat("\n")   
     Phe.more()                  ### this function can be found in 'adjustss.more()'; doing "C -> D" or "D -> C" here
}