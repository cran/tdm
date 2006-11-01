dose.again<-function(a)
{
 cat("\n")
   file.menu <- c("Yes",
                  "No")
   pick <- menu(file.menu, title = "<< Dose adjustment again? >>")
 if (pick == 1){
    a.more()
  } else {
    if (pick == 2){
    cal.again()
    }
    }   
}