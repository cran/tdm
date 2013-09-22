# ask user does he want to calculate another drug again?
cal.again<-function()
{
  cat("\n")
   file.menu <- c("Yes",                                                                  
                  "No")
   pick <- menu(file.menu, title = "<< Do you want to calculate with other drugs? >>")
 if (pick == 1){
    go2menu()
  } else {
    if (pick == 2){
    graphics.off()
    if(file.exists("params.csv")) file.remove("params.csv")
    cat("\n  Thank you for using tdm. Bye now.\n\n") 
    }
    }   
}