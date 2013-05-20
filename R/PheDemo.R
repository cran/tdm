PheDemo<-function(){
     cat("\n")
     cat("---------< Test Runs with Phenytoin PK Parameter Estimation >---------\n")
     cat("\n")
     cat("Scenario: A 45-year old male patient was taking phenytoin 300 mg orally\n")
     cat("every day (24 hr) to treat his epilepsy for more than one month.\n")
     cat("His body weight is 65 Kg. Supposed his plasma phenytoin concentration is\n")
     cat("measured as 7.61 mg/L or 15.1 mg/L or 30.1 mg/L right before next \n")
     cat("dose, we like to estimate his PK parameters (Km and Vmax) based on\n")
     cat("different phenytoin plasma conc.. Dosage adjustment is omitted.\n")
     cat("\n")
     cat("\n")
     note_for_Phe_input()
     cat("\n")
     note_for_close_window()
     PheSSpar<-data.frame(parameter=c("bw (kg)","D (mg)","tau (hr)","c (mg/L)"),value=c(0))
##     PheSSpar<-edit(PheSSpar)
##     PheSSpar<-zcheck(PheSSpar)
     cat("\n")
     PheSSpar[4,2]<-7.61
     PheSSpar[3,2]<-24
     PheSSpar[2,2]<-300
     PheSSpar[1,2]<-65
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     convergence_plots_sep()
     note_for_Phe_output()    
     show(samplesStats("*"))
     cat("\n")
     C<-Phecpr(PheSSpar[2,2],PheSSpar[3,2])
     Final<-(c(matrix(C[1 ,1]), PheSSpar[4,2]))
     coutput<-data.frame(Final)
     row.names(coutput)<-list("C(ss_calc)","C(ss_obs)")
     show(coutput)
     cat("\n")
     cat("\n")
     PheSSpar[4,2]<-15.1
     PheSSpar[3,2]<-24
     PheSSpar[2,2]<-300
     PheSSpar[1,2]<-65
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     convergence_plots_sep()
     note_for_Phe_output()    
     show(samplesStats("*"))
     cat("\n")
     C<-Phecpr(PheSSpar[2,2],PheSSpar[3,2])
     Final<-(c(matrix(C[1 ,1]), PheSSpar[4,2]))
     coutput<-data.frame(Final)
     row.names(coutput)<-list("C(ss_calc)","C(ss_obs)")
     show(coutput)
     cat("\n")
     cat("\n")
     PheSSpar[4,2]<-30.1
     PheSSpar[3,2]<-24
     PheSSpar[2,2]<-300
     PheSSpar[1,2]<-65
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     convergence_plots_sep()
     note_for_Phe_output()    
     show(samplesStats("*"))
     cat("\n")
     C<-Phecpr(PheSSpar[2,2],PheSSpar[3,2])
     Final<-(c(matrix(C[1 ,1]), PheSSpar[4,2]))
     coutput<-data.frame(Final)
     row.names(coutput)<-list("C(ss_calc)","C(ss_obs)")
     show(coutput)
     cat("\n")
}