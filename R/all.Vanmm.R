all.Vanmm<-function(){
     cat("\n")
     note_for_Vansm_input()
     cat("\n")
     note_for_close_window()
     VanMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),bw=c(0),Scr=c(0),D=c(0),tau=c(0),tin=c(0))
     VanMMpar<-edit(VanMMpar)
##     VanMMpar<-ymscheck(VanMMpar)
     cat("\n")
     note_for_Vansm_conc_input()
     cat("\n")
     note_for_close_window()
     VanMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     VanMMMpar<-edit(VanMMMpar)
     VanMMMpar<-mscheck(VanMMMpar)
     for(i in 1:length(unique(VanMMpar$subject))){
     k=length(VanMMMpar$ts[VanMMMpar$subject==i])
     A=VanMMMpar$conc[VanMMMpar$subject==i]
     B=VanMMMpar$ts[VanMMMpar$subject==i]
     d=VanMMpar$tau[VanMMpar$subject==i]
     e=VanMMpar$tin[VanMMpar$subject==i]
     f=VanMMpar$D[VanMMpar$subject==i]
     g=VanMMpar$bw[VanMMpar$subject==i]
     h=VanMMpar$Scr[VanMMpar$subject==i]
     l=VanMMpar$Gender[VanMMpar$subject==i]
     J=VanMMpar$age[VanMMpar$subject==i]
     Van.mm(k,A,B,d,e,f,g,h,l,J,i)
     note_for_convergence_plots()
     cat("\n")
     cat("==========================================================================\n")
     cat("                        << Subject",i,">>                           \n\n" )
     note_for_Vansm_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     C<-infcpr(f,e,d,d)/(exp(-(samplesStats("cl")/samplesStats("v"))*(d-(e+1))))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")
     show(coutput)
     C<-infcpr(f,e,d,d)
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