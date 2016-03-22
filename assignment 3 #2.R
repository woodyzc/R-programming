rankhospital<- function(state,disease,num){
  alldata<- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv',colClasses = "character")
  if (disease=='Heart attack'){
    colnum<- 11
  }
  if (disease=='Heart failure'){
    colnum<- 17
  }
  if (disease=='pneumonia'){
    colnum<- 23
  }
  allstateDate<- split(alldata,alldata[7])
  stateDate<- as.data.frame(allstateDate[state])
  diseasedata<- as.numeric(stateDate[,colnum])
  diseasedata.compcase<- complete.cases(diseasedata)
  sorteddata<- order(as.numeric(stateDate[diseasedata.compcase,colnum]))
  list<- data.frame(hospital=stateDate[diseasedata.compcase,2], disease=as.numeric(stateDate[diseasedata.compcase,colnum]))
  list<- list[order(list[,2],list[,1]),]
  print (list)
  if (num=='best'){
    print (list[1,])
  }
  else if (num=='worst'){
    print (list[length(list[,1]),])
  }
  else {
    if (num>length(sorteddata)){
      print ('NA')
    }
    else {
    print (list[num,])
    }
  }
  
}