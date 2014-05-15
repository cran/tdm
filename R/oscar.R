# List of Drugs Menu
oscar<-function()
{
  cat("\n")
  graphics.off()
  options(digits=5)
  file.menu <- c("** Buprenorphine (NA)",          
                 "Codeine",
                 "Fentanyl",
                 "Hydrocodone",
                 "Hydromorphone",
                 "Meperidine",
                 "Methadone",
                 "Morphine",
                 "Oxycodone IR",
                 "Oxymorphone",
                 "Tramadol")
  pick <- menu(file.menu, title = "<< Opioids List (TDM), Select '0' to quit.>>")
  if (pick == 1){
     cat("\n\n")
     ### all.OO.sm(OOlist="bupr")
     readline(" This opioid is still not available yet. Press Enter to continue.\n\n")
     oscar()
  } else {
    if (pick == 2){
      cat("\n\n") 
      all.OO.sm(OOlist="codn")
    } else {
    if (pick == 3){
      cat("\n\n") 
      all.OO.sm(OOlist="fent")
    }  else {
    if (pick == 4){
      cat("\n\n") 
      all.OO.sm(OOlist="hc")
    }  else {
    if (pick == 5){
      cat("\n\n") 
      all.OO.sm(OOlist="hm")
    } else {
    if (pick == 6){
      cat("\n\n") 
      all.OO.sm(OOlist="mepr")
    } else {
    if (pick == 7){
      cat("\n\n") 
      all.OO.sm(OOlist="metha")
    } else {
    if (pick == 8){
      cat("\n\n") 
      all.OO.sm(OOlist="morp")
    } else {
    if (pick == 9){
      cat("\n\n") 
      all.OO.sm(OOlist="ocIR")
    } else {
    if (pick == 10){
      cat("\n\n") 
      all.OO.sm(OOlist="om")
    } else {
    if (pick == 11){
      cat("\n\n") 
      all.OO.sm(OOlist="trama")
    } else {
      if (pick == 0){
        if(file.exists("params.csv")) file.remove("params.csv")
        graphics.off()
        cat("\n  Thank you for using tdm. Bye now. \n\n")    
       }
     }
    }
    }
    }
    }
    }
    }
    }
  }
 }
}
}
