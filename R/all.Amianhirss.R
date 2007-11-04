all.Amianhirss<-function(){
     cat("\n")                                                                       # show input and output information make user convenience to use                                                                        
     cat("--------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR input data information--     \n")
     note_for_Theirss_input()
     cat("--------------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     AmianhirSSpar<-data.frame(parameter=c("Gender","age (yr)","ht (cm)","CHF","smoke","D (mg)","tau (hr)","ts (hr)","c (mg/L)"),value=c(0))      # edit table of aminophylline anhydrous input data information                                               
     AmianhirSSpar<-edit(AmianhirSSpar)                                                                                                           # show table of aminophylline anhydrous input data information for user editing                                                                                                                                           
     AmianhirSSpar<-ycheck(AmianhirSSpar)                                                                                                         # avoid user missing input information
     cat("\n")                                                                                                                                    # 空一行
     Amianhir.ss(AmianhirSSpar[9,2],AmianhirSSpar[7,2],AmianhirSSpar[8,2],AmianhirSSpar[6,2],AmianhirSSpar[3,2],AmianhirSSpar[2,2],AmianhirSSpar[5,2],AmianhirSSpar[1,2],AmianhirSSpar[4,2])    # calculate individual aminophylline anhydrous PK parameters and show its prediction
     note_for_convergence_plots()
     convergence_plots_sep()
     cat("------------------------------------------------------------------\n")
     cat("    --Aminophylline anhydrous IR output data information--        \n")
     note_for_Theirss_output()
     cat("------------------------------------------------------------------\n")
     show(samplesStats("*"))                                                 # show predicted PK parameters of aminophylline anhydrous 
     cat("\n") 
     C<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],AmianhirSSpar[8,2])     # calculate predicted steady-state measured concentration of aminophylline anhydrous (equation of _phylline IR concentration)
     sim<-matrix(C[1 ,1])                                                           # 取表格[1,1]之答案
     coutput<-data.frame(sim)                                                       # 命名所取出來的[1,1]為couput
     colnames(coutput)<-list("Cmss_pr (mg/L)")                                      # 並命名此欄位為Cmss_pr(mg/L)
     output1<-coutput                                                               # 將Cmss_pr命名為coutput1   
     show(coutput)                                                                  # show the concentration of aminophylline anhydrous  
     r=log(1.85-(samplesStats("cl_F")/samplesStats("v_F")))/(1.85-(samplesStats("cl_F")/samplesStats("v_F")))      # 計算tmax
     C<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],r)                                                     # calculate predicted steady-state peak concentration of aminophylline anhydrous
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Cpss_pr (mg/L)")                                                                     # 並命名此欄為為Cpss_pr(mg/L)
     output2<-coutput                                                                                              # 將Cmss_pr命名為coutput2 
     show(coutput)
     C<-TheIRsscpr(0.85,AmianhirSSpar[6,2],AmianhirSSpar[7,2],AmianhirSSpar[7,2])                                    # calculate predicted steady-state trough concentration of aminophylline anhydrous
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Ctss_pr (mg/L)")                                                                     # 並命名此欄為為Ctss_pr(mg/L)
     output3<-coutput                                                                                              # 將Cmss_pr命名為coutput3
     show(coutput)     
     cat("\n")    
     Amianhir.more()                                           # calculate dose adjustment of Aminoflycoside with single subject single concentration                                                                                                                                                            
}