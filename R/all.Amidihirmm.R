all.Amidihirmm<-function(){
     cat("\n")                                                                       
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous IR input data information--    \n")
     note_for_Theirsm_input()
     cat("-------------------------------------------------------------\n") 
     cat("\n")
     note_for_close_window()
     AmidihirMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),D=c(0),tau=c(0))
     AmidihirMMpar<-edit(AmidihirMMpar)
     AmidihirMmpar<-ymscheck(AmidihirMMpar)
     cat("\n")
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous IR input data information--    \n")
     note_for_Thesm_conc_input()
     cat("-------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmidihirMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     AmidihirMMMpar<-edit(AmidihirMMMpar)
     AmidihirMMMpar<-mscheck(AmidihirMMMpar)
     for(i in 1:length(unique(AmidihirMMpar$subject))){
     a=length(AmidihirMMMpar$ts[AmidihirMMMpar$subject==i])
     b=AmidihirMMMpar$conc[AmidihirMMMpar$subject==i]
     C=AmidihirMMMpar$ts[AmidihirMMMpar$subject==i]
     d=AmidihirMMpar$tau[AmidihirMMpar$subject==i]
     e=AmidihirMMpar$D[AmidihirMMpar$subject==i]
     f=AmidihirMMpar$ht[AmidihirMMpar$subject==i]
     g=AmidihirMMpar$age[AmidihirMMpar$subject==i]
     h=AmidihirMMpar$smoke[AmidihirMMpar$subject==i]
     l=AmidihirMMpar$Gender[AmidihirMMpar$subject==i]
     J=AmidihirMMpar$CHF[AmidihirMMpar$subject==i]
     Amidihir.mm(a,b,C,d,e,f,g,h,l,J,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("------------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous IR input data information--         \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(0.8,e,d,r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     show(coutput) 
     C<-TheIRsscpr(0.8,e,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)     
     cat("=============================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()         # ask user does he want to calculate another drug again?
}