pListPrint=function(pList){
  
  numPlots = length(pList)
  cols=min(numPlots,2)
  plotCols = cols                      
  plotRows = ceiling(numPlots/plotCols)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows,plotCols)))
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(pList[[i]], vp = vplayout(curRow, curCol ))
  }
  
}