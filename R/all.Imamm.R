all.Imamm<-function(){
     cat("\n")
     note_for_Imasm_input()
     cat("\n")
     note_for_close_window()
     ImaMMpar<-data.frame(subject=c(1,2),BW=c(0),OCC=c(0),Hb=c(0),WBC=c(0),D=c(0),tau=c(0))
     ImaMMpar<-edit(ImaMMpar)
     ImaMMpar<-ymscheck(ImaMMpar)
     cat("\n")
     note_for_Imasm_conc_input()
     cat("\n")
     cat("****************************************************************\n")
     cat("      ATTENTION :                                               \n")
     cat("      Sampling Time (ts) must be greater than 1.5 hr.           \n")
     cat("****************************************************************\n")
     cat("\n\n")
     cat("          Pressing Enter to continue..                          \n")
     readline()
     cat("\n")
     note_for_close_window()
     ImaMMMpar<-data.frame(subject=c(1),n=c(0),ts=c(0),conc=c(0))
     ImaMMMpar<-edit(ImaMMMpar)
     ImaMMMpar<-mscheck(ImaMMMpar)
     for(i in 1:length(unique(ImaMMpar$subject))){
     J=length(ImaMMMpar$ts[ImaMMMpar$subject==i])
     A=ImaMMMpar$conc[ImaMMMpar$subject==i]
     B=ImaMMMpar$ts[ImaMMMpar$subject==i]
     l=ImaMMpar$tau[ImaMMpar$subject==i]
     d=ImaMMMpar$n[ImaMMMpar$subject==i]
     e=ImaMMpar$D[ImaMMpar$subject==i]
     f=ImaMMpar$OCC[ImaMMpar$subject==i]
     g=ImaMMpar$BW[ImaMMpar$subject==i]
     h=ImaMMpar$Hb[ImaMMpar$subject==i]
     k=ImaMMpar$WBC[ImaMMpar$subject==i]
     Ima.mm(J,A,B,l,d,e,f,g,h,k,i)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                       << Subject",i,">>                           \n\n" )
     note_for_Imasm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Imasscpr(e,l)
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