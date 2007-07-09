all.Tacmm<-function(){
     cat("\n")
     note_for_Tacsm_input()
     cat("\n")
     note_for_close_window()
     TacMMpar<-data.frame(subject=c(1,2),Hem=c(0),Alb=c(0),Dil=c(0),Flu=c(0),D=c(0),tau=c(0))
     TacMMpar<-edit(TacMMpar)
     TacMMpar<-ymscheck(TacMMpar)
     cat("\n")
     note_for_Tacsm_conc_input()
     cat("\n")
     note_for_close_window()
     TacMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     TacMMMpar<-edit(TacMMMpar)
     TacMMMpar<-mscheck(TacMMMpar)
     for(i in 1:length(unique(TacMMpar$subject))){
     J=length(TacMMMpar$ts[TacMMMpar$subject==i])
     A=TacMMMpar$conc[TacMMMpar$subject==i]
     B=TacMMMpar$ts[TacMMMpar$subject==i]
     d=TacMMpar$tau[TacMMpar$subject==i]
     e=TacMMpar$D[TacMMpar$subject==i]
     f=TacMMpar$Hem[TacMMpar$subject==i]
     g=TacMMpar$Alb[TacMMpar$subject==i]
     h=TacMMpar$Dil[TacMMpar$subject==i]
     l=TacMMpar$Flu[TacMMpar$subject==i]
     Tac.mm(J,A,B,d,e,f,g,h,l,i)
     note_for_convergence_plots()
     cat("\n")
     cat("**************************************************************************\n")
     cat("                        << Subject",i,">>                           \n\n" )
     note_for_Tacsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Taccpr(4.5,e,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mcg/mL)")
     show(coutput) 
     cat("\n**************************************************************************\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}