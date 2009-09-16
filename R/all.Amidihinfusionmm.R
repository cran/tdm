all.Amidihinfusionmm<-function(){
     cat("\n")                                                                       
     cat("-----------------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous iv infusion input data information--     \n")
     note_for_Thesm_conc_input()
     cat("-----------------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmidihinfusionMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),DL=c(0),tinf=c(0),R=c(0))
     AmidihinfusionMMpar<-edit(AmidihinfusionMMpar)
##     AmidihinfusionMMpar<-wmscheck(AmidihinfusionMMpar)
     cat("\n")
     cat("------------------------------------------------------------------------\n")
     cat("    --Aminophylline dihydrous iv infusion input data information--      \n")
     note_for_Theinfusionsm_input()
     cat("------------------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmidihinfusionMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))
     AmidihinfusionMMMpar<-edit(AmidihinfusionMMMpar)
     AmidihinfusionMMMpar<-mscheck(AmidihinfusionMMMpar)
     for(i in 1:length(unique(AmidihinfusionMMpar$subject))){
     a=length(AmidihinfusionMMMpar$ts[AmidihinfusionMMMpar$subject==i])
     b=AmidihinfusionMMMpar$conc[AmidihinfusionMMMpar$subject==i]
     C=AmidihinfusionMMMpar$ts[AmidihinfusionMMMpar$subject==i]
     e=AmidihinfusionMMpar$DL[AmidihinfusionMMpar$subject==i]
     k=AmidihinfusionMMpar$R[AmidihinfusionMMpar$subject==i]
     m=AmidihinfusionMMpar$tinf[AmidihinfusionMMpar$subject==i]
     f=AmidihinfusionMMpar$ht[AmidihinfusionMMpar$subject==i]
     g=AmidihinfusionMMpar$age[AmidihinfusionMMpar$subject==i]
     h=AmidihinfusionMMpar$smoke[AmidihinfusionMMpar$subject==i]
     l=AmidihinfusionMMpar$Gender[AmidihinfusionMMpar$subject==i]
     J=AmidihinfusionMMpar$CHF[AmidihinfusionMMpar$subject==i]
     Amidihinfusion.mm(a,i,b,C,f,g,h,l,J,e,k,m)
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("------------------------------------------------------------------------\n")
     cat("   --Aminophylline dihydrous iv infusion input data information--      \n")
     note_for_Theinfusionsm_output()
     cat("------------------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     C<-0.8*k/(samplesStats("cl"))
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