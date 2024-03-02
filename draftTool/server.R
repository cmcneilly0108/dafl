# Move a player from FA to a Team, update both lists
# GUI to pick a player

setwd("../code/")
source("./draftGuide.r")
setwd("../draftTool")

teams <- unique(pstandings$Team) 





shinyServer(function(input, output,session) {
  output$pstandings <- renderDataTable({ pstandings })  
  output$injOrig <- renderDataTable({ injOrig })  
  output$topHitters <- renderDataTable({ topHitters })  
  output$prospectH <- renderDataTable({ prospectH })  
  output$prospectP <- renderDataTable({ prospectP })
  output$protectSummary <- renderDataTable({ protectSummary }) 
  output$ppp <- renderDataTable({ ppp }) 
  
  })
