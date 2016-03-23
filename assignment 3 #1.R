best<- function(state,disease){
  x<- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses = "character")
  if (disease=='Heart attack'){
    colnum<- 11
  }
  if (disease=='Heart failure'){
    colnum<- 17
  }
  if (disease=='pneumonia'){
    colnum<- 23
  }
  stateDate<- split(x,x[7])
  stateDate<- as.data.frame(stateDate[state])
  a<- as.numeric(stateDate[,colnum])
  y<- complete.cases(a)
  min<- (min(as.numeric(stateDate[y,colnum])))
  right<- stateDate[stateDate[,colnum]==min,]
  print (right[,c(2)])
  
}