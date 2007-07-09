all.Amimm<-function(){
     cat("\n")
     note_for_Amism_input()
     cat("\n")
     note_for_close_window()
     AmiMMpar<-data.frame(subject=c(1,2),Gender=c(0),age=c(0),bw=c(0),Ht=c(0),Scr=c(0),D=c(0),tau=c(0),tin=c(0))      # edit table of Aminoglycoside input data information except ts and conc 
     AmiMMpar<-edit(AmiMMpar)
     AmiMMpar<-ymscheck(AmiMMpar)
     cat("\n")
     note_for_Amism_conc_input()
     cat("\n")
     note_for_close_window()
     AmiMMMpar<-data.frame(subject=c(1),ts=c(0),conc=c(0))                  # edit table of Aminoglycoside input data information including subject, ts and conc
     AmiMMMpar<-edit(AmiMMMpar)
     AmiMMMpar<-mscheck(AmiMMMpar)
     for(i in 1:length(unique(AmiMMpar$subject))){       # Loop, 為了計算多人多點的參數(選擇計算一個人)
     a=length(AmiMMMpar$ts[AmiMMMpar$subject==i])        # a=number of sampling time of subject[i]
     b=AmiMMMpar$conc[AmiMMMpar$subject==i]              # b=concentration of subject[i]
     C=AmiMMMpar$ts[AmiMMMpar$subject==i]                # c=sampling time of subject[i]
     d=AmiMMpar$tau[AmiMMpar$subject==i]                 # d=dosing interval of subject[i]
     e=AmiMMpar$tin[AmiMMpar$subject==i]                 # e=infusion tiem of subject[i]
     f=AmiMMpar$D[AmiMMpar$subject==i]                   # f=dose of subject[i]  
     g=AmiMMpar$bw[AmiMMpar$subject==i]                  # g=bw of subject[i]
     h=AmiMMpar$Ht[AmiMMpar$subject==i]                  # h=ht of subject[i]
     l=AmiMMpar$Scr[AmiMMpar$subject==i]                 # l=Scr of subject[i]
     J=AmiMMpar$Gender[AmiMMpar$subject==i]              # J=Gender of subject[i]
     k=AmiMMpar$age[AmiMMpar$subject==i]                 # k=age of subject[i]
     Ami.mm(a,b,C,d,e,f,g,h,l,J,k,i)                     # calculate individual Aminoglycoside PK parameters and show its prediction
     note_for_convergence_plots()
     cat("\n")
     cat("==========================================================================\n")
     cat("                          << Subject",i,">>                           \n\n" )
     note_for_Amism_output()
     convergence_plots_sep()
     show(samplesStats("*"))
     cat("\n")
     C<-infcpr(f,e,d,e)                                   
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
     cal.again()         # ask user does he want to calculate another drug again?
}