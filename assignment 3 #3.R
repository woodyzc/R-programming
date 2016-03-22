rankall<- function(disease,num='best'){
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
  new_data<- list()
  for (state in 1:length(allstateDate)){
    stateData<- as.data.frame(allstateDate[state])
    comp.diseaseData<- complete.cases(as.numeric(stateData[,colnum]))
    stateData<- stateData[comp.diseaseData,]
    list<- data.frame(hospital=stateData[,2], state=stateData[,7],disease=as.numeric(stateData[,colnum]))
    list<- list[order(list[,3],list[,1]),]
    stateName<- stateData[1,7]
    new_data[[stateName]]<- list
  }
  if (num=='best'){
    for (state in new_data){
      print (as.data.frame(state)[1,])
    }
  }
  else if (num=='worst'){
    for (state in new_data){
      print (as.data.frame(state)[length(as.data.frame(state)[,1]),])
    }
  }
  else {
      for (state in new_data){
        if (nrow(as.data.frame(state))< num){
          print ('NA',as.data.frame(state)[1,2])
        } else{
        print (as.data.frame(state)[num,])
    }
  }
  }
  }