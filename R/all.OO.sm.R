all.OO.sm<-function(OOlist="ocIR"){

phenotype<-NULL
cl_ocIR_init<-NULL
ka_ocIR_fixed<-NULL
vd_ocIR_init<-NULL
Tau<-NULL
MSC<-NULL
MEC<-NULL
TTC<-NULL
Cav<-NULL
Cav<<-0
ka<-NULL
x_label<-""
y_label<-""
main_title<-""
is.Fentanyl<-NULL
is.Fentanyl<<-FALSE


     cat("\n")
     if(OOlist=="ocIR") note_for_ocIR_input()
                   else note_for_OO_input()
     cat("\n")
     note_for_close_window()
     if(OOlist=="ocIR"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)","Phenotype"),value=c(10,2))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       phenotype<-ocIRSMMpar[2,2]
       if(phenotype==1) {
          cl_ocIR_init<-41.3}   ## UM: 0.59 L/hr/kg*70 kg BW
       else if(phenotype==2){
          cl_ocIR_init<-35.7}   ## EM: 0.51 L/hr/kg*70 kg BW
       else if(phenotype==3){
          cl_ocIR_init<-26  }   ## PM: 0.37 L/hr/kg*70 kg BW
          ka_ocIR_fixed<-3.5
          vd_ocIR_init<-196     ## Vd: 2.8 L/kg*70kg BW = 196
       Tau<<-6
       MEC<<-10
       MSC<<-100
       y_label<<-"Oxycodone plasma conc. (ng/mL)"
       main_title<<-"Oxycodone IR TDM"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="bupr"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(16))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 9.1
       ka_ocIR_fixed<- 0.52
       vd_ocIR_init <- 266
       Tau<<-8
       MEC<<-1
       MSC<<-5
       y_label<<-"Buprenorphine plasma conc. (ng/mL)"
       main_title<<-"Buprenorphine IR TDM"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="codn"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(30))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 86
       ka_ocIR_fixed<- 15.6
       vd_ocIR_init <- 210
       Tau<<-6
       MEC<<-10
       MSC<<-100
       y_label<<-"Codeine plasma conc. (ng/mL)"
       main_title<<-"Codeine TDM"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="mepr"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(100))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 70
       ka_ocIR_fixed<- 4.8
       vd_ocIR_init <- 280
       Tau<<-4
       MEC<<-400
       MSC<<-700
       main_title<<-"Meperidine TDM"
       y_label<<-"Meperidine plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="hc"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)","Phenotype"),value=c(10,1))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       phenotype<-ocIRSMMpar[2,2]
       if(phenotype==1) {
          vd_ocIR_init<-250     ## Vd: 3.58 L/kg*70kg BW = 196
          cl_ocIR_init<-58.8}   ## UM: 0.84 L/hr/kg*70 kg BW
       else if(phenotype==2){
          vd_ocIR_init<-270     ## Vd: 3.85 L/kg*70kg BW = 196
          cl_ocIR_init<-41.3}   ## EM: 0.59 L/hr/kg*70 kg BW
       else if(phenotype==3){
          vd_ocIR_init<-231     ## Vd: 2.8 L/kg*70kg BW = 196
          cl_ocIR_init<-24.5  } ## PM: 0.35 L/hr/kg*70 kg BW
       ka_ocIR_fixed<-2.8
       Tau<<-6
       MEC<<-10
       MSC<<-100
       main_title<<-"Hydrocodone TDM"
       y_label<<-"Hydrocodone plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="hm"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(4))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 86
       ka_ocIR_fixed<- 15.6
       vd_ocIR_init <- 210
       Tau<<-6
       MEC<<-1
       MSC<<-5
       main_title<<-"Hydromorphone TDM"
       y_label<<-"Hydromorphone plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="metha"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(10))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 13.3
       ka_ocIR_fixed<- 0.58
       vd_ocIR_init <- 385
       Tau<<-4
       MEC<<-100
       MSC<<-400
       main_title<<-"Methadone TDM"
       y_label<<-"Methadone plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="morp"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(30))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 100
       ka_ocIR_fixed<- 15.6
       vd_ocIR_init <- 315
       Tau<<-4
       MEC<<-21
       MSC<<-65
       main_title<<-"Morphine TDM"
       y_label<<-"Morphine plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="om"){  ### ref.http://www.eperc.mcw.edu/EPERC/FastFactsIndex/ff_181.htm: dosage etc.
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(10))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 28
       ka_ocIR_fixed<- 3.1
       vd_ocIR_init <- 210
       Tau<<-6
       MEC<<-0.65
       MSC<<-8.29
       main_title<<-"Oxymorphone TDM"
       y_label<<-"Oxymorphone plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="trama"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mg)"),value=c(100))
       ocIRSMMpar<-edit(ocIRSMMpar)
       Dose<-ocIRSMMpar[1,2]*1000  ### unit conversion mg -> mcg
       cl_ocIR_init <- 26
       ka_ocIR_fixed<- 2.6
       vd_ocIR_init <- 196         ### 2.8 L/kg * 70 kg BW
       Tau<<-6
       MEC<<-100
       MSC<<-1000
       main_title<<-"Tramadol TDM"
       y_label<<-"Tramadol plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
     }
     if(OOlist=="fent"){
       ocIRSMMpar<-data.frame(parameter=c("Dose (mcg)"),value=c(200))
       ocIRSMMpar<-edit(ocIRSMMpar)
       ### Dose<-ocIRSMMpar[1,2]*1000  ### no need for fentanyl
       Dose<-ocIRSMMpar[1,2]
       cl_ocIR_init <- 92.4
       ka_ocIR_fixed<- 2.9
       vd_ocIR_init <- 385         ### 5.5 L/kg * 70 kg BW
       Tau<<-6
       MEC<<-3
       MSC<<-300
       main_title<<-"Fentanyl TDM"
       y_label<<-"Fentanyl plasma conc. (ng/mL)"
       x_label<-"Time after dosing (hr)"
       is.Fentanyl<<-TRUE
     }
     
     ka<<-ka_ocIR_fixed
     Tau<-Tau
     
     cat("\n Input data are as follows:\n")
     cat(" --------------------------\n")
     show(ocIRSMMpar);cat("\n\n")
     cat("\n")
     note_for_close_window()
     cat("\n")
     note_for_OO_conc_input()

     if(OOlist=="ocIR")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(42.6,15.5)) ### fixed two data points entry here. can be adjusted.
     if(OOlist=="bupr")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(24.0,51.4))
     if(OOlist=="codn")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(100.4,15.2))
     if(OOlist=="fent")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(0.443,0.230))
     if(OOlist=="hc")    ocIRSMpar<-data.frame(ts=c(1,6),conc=c(34.0,18.1))
     if(OOlist=="hm")    ocIRSMpar<-data.frame(ts=c(1,6),conc=c(16.5,7.63))
     if(OOlist=="mepr")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(292,88.0))
     if(OOlist=="metha") ocIRSMpar<-data.frame(ts=c(1,6),conc=c(11.2,22.1))
     if(OOlist=="morp")  ocIRSMpar<-data.frame(ts=c(1,6),conc=c(70.7,14.4))
     if(OOlist=="om")    ocIRSMpar<-data.frame(ts=c(1,6),conc=c(41.8,24.8))
     if(OOlist=="trama") ocIRSMpar<-data.frame(ts=c(1,6),conc=c(431,243))

     ### ocIRSMpar<-data.frame(ts=c(0),conc=c(0))   ### un-remark this for final release. -YJ
     ocIRSMpar<<-edit(ocIRSMpar)
     cat("\n Input conc. are as follows:\n")
     cat(" --------------------------\n")
     show(ocIRSMpar);cat("\n\n")
     cat("\n")
     oc.IR.sm(length(ocIRSMpar$ts),ocIRSMpar$conc,ocIRSMpar$ts,Dose,cl_ocIR_init,vd_ocIR_init,ka_ocIR_fixed)
     note_for_convergence_plots()
     note_for_OO_output()
     ### show obs. & calc. conc. here  -YJ
     
     cat("\n\n")
     for(i in 1:length(ocIRSMpar$ts)){
         Cx<-OOsdcpr(Dose,ocIRSMpar$ts[i]) ## 24 hr, qd po, ka= 3.7 ### re-check eq. for OOsdcpr here
         coutput<-data.frame(conc=c("obs. conc.","calc. conc."), values=c(ocIRSMpar$conc[i],Cx))
         cat("--- Drug plasma conc. (ng/mL) at Time =",ocIRSMpar$ts[i],"---\n")
         show(coutput);cat("\n")
     }

     sdToss(Dose)
     plots_tdm.SD(Dose)
     ocIR.more()                # in 'adjust.oscar()'; doing 'C -> D' or 'D -> C'
}
