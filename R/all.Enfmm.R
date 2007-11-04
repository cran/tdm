all.Enfmm<-function(){
     cat("\n")
     note_for_Enfsm_input()
     cat("\n")
     note_for_close_window()
     EnfMMpar<-data.frame(subject=c(1,2),Gender=c(0),bw=c(0),D=c(0),tau=c(0))
     EnfMMpar<-edit(EnfMMpar)
     EnfMMpar<-zmscheck(EnfMMpar)
     cat("\n")
     note_for_Enfsm_conc_input()
     cat("\n")
     note_for_close_window()
     EnfMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     EnfMMMpar<-edit(EnfMMMpar)
     EnfMMMpar<-mscheck(EnfMMMpar)
     for(i in 1:length(unique(EnfMMpar$subject))){
     h=length(EnfMMMpar$ts[EnfMMMpar$subject==i])
     A=EnfMMMpar$conc[EnfMMMpar$subject==i]
     B=EnfMMMpar$ts[EnfMMMpar$subject==i]
     d=EnfMMpar$tau[EnfMMpar$subject==i]
     e=EnfMMpar$D[EnfMMpar$subject==i]
     f=EnfMMpar$bw[EnfMMpar$subject==i]
     g=EnfMMpar$Gender[EnfMMpar$subject==i]
     Enf.mm(h,A,B,d,e,f,g,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                    << Subject",i,">>                           \n\n" )
     note_for_Enfsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-pocpr(samplesStats("ka"),e,d,d)
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