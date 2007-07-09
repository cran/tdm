all.Oxtirsm<-function(){
     cat("\n")                                                                      
     cat("--------------------------------------------------------\n")
     cat("    --Oxtriphylline IR input data information--         \n")
     note_for_Theirsm_input()
     cat("--------------------------------------------------------\n") 
     cat("\n")
     note_for_close_window()
     OxtirSMMpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)"),value=c(0))
     OxtirSMMpar<-edit(OxtirSMMpar)
     OxtirSMMpar<-ycheck(OxtirSMMpar)
     cat("\n")
     cat("---------------------------------------------------------\n")
     cat("    --Oxtriphylline IR input data information--          \n")
     note_for_Thesm_conc_input()
     cat("---------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     OxtirSMpar<-data.frame(ts=c(0),conc=c(0))                                                                                                                             
     OxtirSMpar<-edit(OxtirSMpar)                                               
     OxtirSMpar<-mscheck(OxtirSMpar)                                           
     cat("\n")
     Oxtir.sm(length(OxtirSMpar$ts),OxtirSMpar$conc,OxtirSMpar$ts,OxtirSMMpar[7,2],OxtirSMMpar[6,2],OxtirSMMpar[3,2],OxtirSMMpar[2,2],OxtirSMMpar[5,2],OxtirSMMpar[1,2],OxtirSMMpar[4,2])
     note_for_convergence_plots()
     convergence_plots_sep()
     cat("------------------------------------------------------------------\n")
     cat("    --Oxtriphylline IR output data information--                  \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     show(samplesStats("*"))
     cat("\n")
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(0.65,OxtirSMMpar[6,2],OxtirSMMpar[7,2],r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     output1<-coutput
     show(coutput)
     C<-TheIRsscpr(0.65,OxtirSMMpar[6,2],OxtirSMMpar[7,2],OxtirSMMpar[7,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     output2<-coutput
     show(coutput)     
     cat("\n")   
     Oxtir.more()                                    
}