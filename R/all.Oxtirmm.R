all.Oxtirmm<-function(){
     cat("\n")                                                                      
     cat("--------------------------------------------------------\n")
     cat("    --Oxtriphylline IR input data information--         \n")
     note_for_Theirsm_input()
     cat("--------------------------------------------------------\n") 
     cat("\n")
     note_for_close_window()
     OxtirMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),D=c(0),tau=c(0))
     OxtirMMpar<-edit(OxtirMMpar)
##     OxtirMMpar<-ymscheck(OxtirMMpar)
     cat("\n")
     cat("\n")
     cat("---------------------------------------------------------\n")
     cat("    --Oxtriphylline IR input data information--          \n")
     note_for_Thesm_conc_input()
     cat("---------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     OxtirMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     OxtirMMMpar<-edit(OxtirMMMpar)
     OxtirMMMpar<-mscheck(OxtirMMMpar)
     for(i in 1:length(unique(OxtirMMpar$subject))){
     a=length(OxtirMMMpar$ts[OxtirMMMpar$subject==i])
     b=OxtirMMMpar$conc[OxtirMMMpar$subject==i]
     C=OxtirMMMpar$ts[OxtirMMMpar$subject==i]
     d=OxtirMMpar$tau[OxtirMMpar$subject==i]
     e=OxtirMMpar$D[OxtirMMpar$subject==i]
     f=OxtirMMpar$ht[OxtirMMpar$subject==i]
     g=OxtirMMpar$age[OxtirMMpar$subject==i]
     h=OxtirMMpar$smoke[OxtirMMpar$subject==i]
     l=OxtirMMpar$Gender[OxtirMMpar$subject==i]
     J=OxtirMMpar$CHF[OxtirMMpar$subject==i]
     Oxtir.mm(a,b,C,d,e,f,g,h,l,J,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("------------------------------------------------------------------\n")
     cat("    --Oxtriphylline IR output data information--                  \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(0.65,e,d,r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     show(coutput)
     C<-TheIRsscpr(0.65,e,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)     
     cat("=============================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}