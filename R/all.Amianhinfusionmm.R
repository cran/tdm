all.Amianhinfusionmm<-function(){
     cat("\n")                                                                       
     cat("-----------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous iv infusion input data information--     \n")
     note_for_Thesm_conc_input()
     cat("-----------------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhinfusionMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),DL=c(0),tinf=c(0),R=c(0))
     AmianhinfusionMMpar<-edit(AmianhinfusionMMpar)
     AmianhinfusionMMpar<-wmscheck(AmianhinfusionMMpar)
     cat("\n")
     cat("------------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous iv infusion input data information--      \n")
     note_for_Theinfusionsm_input()
     cat("------------------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhinfusionMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     AmianhinfusionMMMpar<-edit(AmianhinfusionMMMpar)
     AmianhinfusionMMMpar<-mscheck(AmianhinfusionMMMpar)
     for(i in 1:length(unique(AmianhinfusionMMpar$subject))){
     a=length(AmianhinfusionMMMpar$ts[AmianhinfusionMMMpar$subject==i])
     b=AmianhinfusionMMMpar$conc[AmianhinfusionMMMpar$subject==i]
     C=AmianhinfusionMMMpar$ts[AmianhinfusionMMMpar$subject==i]
     e=AmianhinfusionMMpar$DL[AmianhinfusionMMpar$subject==i]
     k=AmianhinfusionMMpar$R[AmianhinfusionMMpar$subject==i]
     m=AmianhinfusionMMpar$tinf[AmianhinfusionMMpar$subject==i]
     f=AmianhinfusionMMpar$ht[AmianhinfusionMMpar$subject==i]
     g=AmianhinfusionMMpar$age[AmianhinfusionMMpar$subject==i]
     h=AmianhinfusionMMpar$smoke[AmianhinfusionMMpar$subject==i]
     l=AmianhinfusionMMpar$Gender[AmianhinfusionMMpar$subject==i]
     J=AmianhinfusionMMpar$CHF[AmianhinfusionMMpar$subject==i]
     Amianhinfusion.mm(a,i,b,C,f,g,h,l,J,e,k,m)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("------------------------------------------------------------------------\n")
     cat("   --Aminophylline anhydrous iv infusion output data information--      \n")
     note_for_Theinfusionsm_output()
     cat("------------------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     C<-0.85*k/(samplesStats("cl"))
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