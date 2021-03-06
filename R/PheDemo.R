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
     PheSSpar<-data.frame(parameter=c("bw (kg)","D (mg)","tau (hr)","c (mg/L)"),value=c(65,300,24,7.61))
     cat("\n")
     cat("--- Phenytoin: Case Study #1 ---\n\n")
     show(PheSSpar);cat("\n\n")
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     note_for_Phe_output()
     PheSSpar<-data.frame(parameter=c("bw (kg)","D (mg)","tau (hr)","c (mg/L)"),value=c(65,300,24,15.1))
     cat("\n")
     cat("--- Phenytoin: Case Study #2 ---\n\n")
     show(PheSSpar);cat("\n\n")
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     note_for_Phe_output()
     PheSSpar<-data.frame(parameter=c("bw (kg)","D (mg)","tau (hr)","c (mg/L)"),value=c(65,300,24,30.1))
     cat("\n")
     cat("--- Phenytoin: Case Study #3 ---\n\n")
     show(PheSSpar);cat("\n\n")
     Phe.ss(PheSSpar[4,2],PheSSpar[3,2],PheSSpar[2,2],PheSSpar[1,2])
     note_for_convergence_plots()
     note_for_Phe_output()
     cat("\n\n")
     unlink("params.csv")
}