all.Valmm<-function(){
     cat("\n")
     note_for_Valsm_input()
     cat("\n")
     note_for_close_window()
     ValMMpar<-data.frame(subject=c(1,2),age=c(0),INDI=c(0),CBZ=c(0),ka=c(0),D=c(0),tau=c(0))
     ValMMpar<-edit(ValMMpar)
##     ValMMpar<-ymscheck(ValMMpar)
     cat("\n")
     note_for_Valsm_conc_input()
     cat("\n")
     note_for_close_window()
     ValMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     ValMMMpar<-edit(ValMMMpar)
     ValMMMpar<-mscheck(ValMMMpar)
     for(i in 1:length(unique(ValMMpar$subject))){
     k=length(ValMMMpar$ts[ValMMMpar$subject==i])
     A=ValMMMpar$conc[ValMMMpar$subject==i]
     B=ValMMMpar$ts[ValMMMpar$subject==i]
     d=ValMMpar$tau[ValMMpar$subject==i]
     e=ValMMpar$D[ValMMpar$subject==i]
     f=ValMMpar$age[ValMMpar$subject==i]
     g=ValMMpar$CBZ[ValMMpar$subject==i]
     h=ValMMpar$INDI[ValMMpar$subject==i]
     J=ValMMpar$ka[ValMMpar$subject==i]
     Val.mm(k,A,B,d,e,f,g,h,J,i)
     note_for_convergence_plots()
     cat("\n")
     cat("==========================================================================\n")
     cat("                     << Subject",i,">>                           \n\n")
     note_for_Valsm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n") 
     C<-Valcpr(J,d,d)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)    
     cat("==========================================================================\n")
     cat(date(),"\n")
     cat("\n\n\n\n")
     }
     cal.again()
}