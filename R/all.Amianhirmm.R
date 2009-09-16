all.Amianhirmm<-function(){
     cat("\n")                                                                       
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--    \n")
     note_for_Theirsm_input()
     cat("-------------------------------------------------------------\n")          
     cat("\n")
     note_for_close_window()
     AmianhirMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),ht=c(0),CHF=c(0),smoke=c(0),D=c(0),tau=c(0))      # edit table of aminophylline anhydrous input data information except ts and conc 
     AmianhirMMpar<-edit(AmianhirMMpar)
##     AmianhirMMpar<-ymscheck(AmianhirMMpar)
     cat("\n")
     cat("-------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--    \n")
     note_for_Thesm_conc_input()
     cat("-------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhirMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))                  # edit table of aminophylline anhydrous input data information including subject, ts and conc
     AmianhirMMMpar<-edit(AmianhirMMMpar)
     AmianhirMMMpar<-mscheck(AmianhirMMMpar)
     cat("\n")
     for(i in 1:length(unique(AmianhirMMpar$subject))){           # Loop 為了計算多人多點的參數(選擇計算一個人)
     a=length(AmianhirMMMpar$ts[AmianhirMMMpar$subject==i])       # a=number of sampling time of subject[i]
     b=AmianhirMMMpar$conc[AmianhirMMMpar$subject==i]             # b=concentration of subject[i]
     C=AmianhirMMMpar$ts[AmianhirMMMpar$subject==i]               # c=sampling time of subject[i]
     d=AmianhirMMpar$tau[AmianhirMMpar$subject==i]                # d=dosing interval of subject[i]
     e=AmianhirMMpar$D[AmianhirMMpar$subject==i]                  # e=dose of subject[i]
     f=AmianhirMMpar$ht[AmianhirMMpar$subject==i]                 # f=height of subject[i]
     g=AmianhirMMpar$age[AmianhirMMpar$subject==i]                # g=age of subject[i]
     h=AmianhirMMpar$smoke[AmianhirMMpar$subject==i]              # h=subject[i]是否有抽煙
     l=AmianhirMMpar$Gender[AmianhirMMpar$subject==i]             # l=Gender of subject[i]
     J=AmianhirMMpar$CHF[AmianhirMMpar$subject==i]                # J=subject[i]使否有CHF
     Amianhir.mm(a,b,C,d,e,f,g,h,l,J,i)                           # calculate individual aminophylline anhydrous PK parameters and show its prediction
     note_for_convergence_plots()
     cat("\n")
     cat("=============================================================================\n")
     cat("                 << Subject",i,">>                           \n\n" )
     cat("------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR output data information--        \n")
     note_for_Theirsm_output()
     cat("------------------------------------------------------------------\n")
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))
     C<-TheIRsscpr(0.85,e,d,r)
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")
     show(coutput)  
     C<-TheIRsscpr(0.85,e,d,d)
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