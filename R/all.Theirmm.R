all.Theirmm<-function(){
     cat("\n")                                                                      
     cat("-----------------------------------------------------\n")
     cat("    --Theophylline IR input data information--       \n")
     note_for_Theirsm_input()
     cat("-----------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     TheirMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),D=c(0),tau=c(0))
     TheirMMpar<-edit(TheirMMpar)
     TheirMMpar<-ymscheck(TheirMMpar)
     cat("\n")
     cat("--------------------------------------------------------\n")
     cat("    --Theophylline IR input data information--          \n")
     note_for_Thesm_conc_input()
     cat("--------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     TheirMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     TheirMMMpar<-edit(TheirMMMpar)
     TheirMMMpar<-mscheck(TheirMMMpar)
     for(i in 1:length(unique(TheirMMpar$subject))){
     a=length(TheirMMMpar$ts[TheirMMMpar$subject==i])
     b=TheirMMMpar$conc[TheirMMMpar$subject==i]
     C=TheirMMMpar$ts[TheirMMMpar$subject==i]
     d=TheirMMpar$tau[TheirMMpar$subject==i]
     e=TheirMMpar$D[TheirMMpar$subject==i]
     f=TheirMMpar$ht[TheirMMpar$subject==i]
     g=TheirMMpar$age[TheirMMpar$subject==i]
     h=TheirMMpar$smoke[TheirMMpar$subject==i]
     l=TheirMMpar$Gender[TheirMMpar$subject==i]
     J=TheirMMpar$CHF[TheirMMpar$subject==i]
     Their.mm(a,b,C,d,e,f,g,h,l,J,i)
     note_for_convergence_plots()
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("-----------------------------------------------------------------\n")
     cat("    --Theophylline IR output data information--                  \n")
     note_for_Theirsm_output()
     cat("-----------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(1,e,d,r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)  
     C<-TheIRsscpr(1,e,d,d)
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