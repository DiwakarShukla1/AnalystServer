library(rjson)

#-----------------------------------------------------------------------------------------
#--------------------------------------overall--------------------------------------------
#-----------------------------------------------------------------------------------------

create_json=function(data){
  library(rjson)
  json <- toJSON(data)
  return(json)
}

entityCount=function(data){
  return(nrow(data))
}

todaysEntityCount=function(data){
  return(sum(data$created_at==Sys.Date()))
}

currentMonthEntityCount=function(data){
  return(sum(substr(data$created_at,1,7)==substr(Sys.Date(),1,7)))
}

entityCountLastWeek=function(data){
  data=aggregate(data$count ~ data$created_at, data = data, sum)
  data=data[order(data$'data$created_at',decreasing = TRUE),]
  data$'data$created_at'= as.Date(data$'data$created_at')
  data=head(data,7)
  return(data)
}

#-----------------------------------------------------------------------------------------
#--------------------------------------Advice Pkg-----------------------------------------
#-----------------------------------------------------------------------------------------

# Support Methods
#-----------------

filterAdvice=function(adviceData){
  adviceData=adviceData[order(adviceData$created_at),]
  adviceData=adviceData[adviceData$owner!="maheshgrt007@gmail.com",]
  adviceData=adviceData[adviceData$owner!="sagar.75@hotmail.com",]
  adviceData$name=NULL
  adviceData$visitor=NULL
  adviceData$times=substr(adviceData$created_at, 12, 19);
  adviceData$created_at= as.Date(adviceData$created_at)
  adviceData$spray_date= as.Date(adviceData$spray_date)
  adviceData$description=NULL
  adviceData$count=1
  adviceData$product=substring(adviceData$product,2)
  adviceData$product=substr(adviceData$product, 1, nchar(adviceData$product)-1)
  adviceData$product=strsplit(adviceData$product,',')
  return(adviceData)
}

readAdvice=function(){
  return(filterAdvice(read.csv("http://ec2-54-187-6-49.us-west-2.compute.amazonaws.com:9002/adviceList.csv",header=TRUE,sep="|")))
}

productCollborate=function(data){
  Data <- data$product
  table(sapply(Data,paste,collapse=""))
  return(sum(table(unlist(Data))))
}

# Group Methods
#-----------------

countProductAdviceByDate=function(data,date){
  data=data[data$created_at==date,]
  if(nrow(data)>0)
    return(productCollborate(data))
  else
    return(0)
}

countLastWeekProductAdvice=function(data){
  temp=list()
  i=0
  j=1
  while(i<7){
    temp[[j]]=list('Date'=Sys.Date()-i,'Count'=countProductAdviceByDate(data,Sys.Date()-i))
    i=i+1
    j=j+1
  }
  return(temp)
}

countCurrentMonthProductAdvice=function(data){
  data=data[substr(data$created_at,1,7)==substr(Sys.Date(),1,7),]
  return(productCollborate(data))
}

gettotaladviceoverall=function(data){
  data = aggregate(data$count ~ data$owner, data = data, sum)
  return(create_json(data))
}

getCountOfProductAdvicedOverall=function(adviceData){
  testData <- adviceData$product
  table(sapply(testData,paste,collapse=""))
  return(table(unlist(testData)))
}

getAdviceCountByVisitType=function(adviceData){
  return(aggregate(adviceData$count ~ adviceData$owner + adviceData$visit_type, data = adviceData, sum))
}

# Individual Methods
#-----------------

countTodaysVisitByEmp=function(key,data){
  data=data[data$created_at==Sys.Date(),]
  data=data[data$owner==key,]
  return(nrow(data))
}

countCurrentMonthVisitByEmp=function(key,data){
  data=data[substr(data$created_at,1,7)==substr(Sys.Date(),1,7),]
  data=data[data$owner==key,]
  return(nrow(data))
}

getCountByCropForAdviceByEmp=function(key,adviceData){
  adviceData=aggregate(adviceData$count ~ adviceData$owner + adviceData$crop, data = adviceData, sum)
  adviceData=adviceData[adviceData$'adviceData$owner'==key,]
  return(adviceData)
}

adviceDaysPatternOfEmp=function(key,adviceData){
  q=1
  daysData=list('Wednesday'=0,'Thursday'=0,'Friday'=0,'Saturday'=0,'Sunday'=0,'Monday'=0,'Tuesday'=0)
  while(q<nrow(adviceData))
  {
    if(key == adviceData[q,4])
    {
      day=weekdays(adviceData[q,]$created_at)
      if(day=="Wednesday"){daysData$Wednesday=as.numeric(daysData$Wednesday)+1}
      else if(day=="Thursday"){daysData$Thursday=as.numeric(daysData$Thursday)+1}
      else if(day=="Friday"){daysData$Friday=as.numeric(daysData$Friday)+1}
      else if(day=="Saturday"){daysData$Saturday=as.numeric(daysData$Saturday)+1}
      else if(day=="Sunday"){daysData$Sunday=as.numeric(daysData$Sunday)+1}
      else if(day=="Monday"){daysData$Monday=as.numeric(daysData$Monday)+1}
      else if(day=="Tuesday"){daysData$Tuesday=as.numeric(daysData$Tuesday)+1}
    }
    q=q+1
  }
  return(daysData)
}

adviceTimePatternOfEmp=function(key,adviceData){
  q=1
  timeData=list('f0to1'=0,'f1to2'=0,'f2to3'=0,'f3to4'=0,'f4to5'=0,'f5to6'=0,'f6to7'=0,'f7to8'=0,'f8to9'=0,'f9to10'=0,'f10to11'=0,'f11to12'=0,'f12to13'=0,'f13to14'=0,'f14to15'=0,'f15to16'=0,'f16to17'=0,'f17to18'=0,'f18to19'=0,'f19to20'=0,'f20to21'=0,'f22to22'=0,'f22to23'=0,'f23to24'=0)
  while(q<nrow(adviceData))
  {
    if(key == adviceData[q,4])
    {
      hour=as.numeric(substr(adviceData[q,]$times, 1, 2))
      if(hour==0){timeData$f0to1=as.numeric(timeData$f0to1)+1}
      else if(hour==1){timeData$f1to2=as.numeric(timeData$f1to2)+1}
      else if(hour==2){timeData$f2to3=as.numeric(timeData$f2to3)+1}
      else if(hour==3){timeData$f3to4=as.numeric(timeData$f3to4)+1}
      else if(hour==4){timeData$f4to5=as.numeric(timeData$f4to5)+1}
      else if(hour==5){timeData$f5to6=as.numeric(timeData$f5to6)+1}
      else if(hour==6){timeData$f6to7=as.numeric(timeData$f6to7)+1}
      else if(hour==7){timeData$f7to8=as.numeric(timeData$f7to8)+1}
      else if(hour==8){timeData$f8to9=as.numeric(timeData$f8to9)+1}
      else if(hour==9){timeData$f9to10=as.numeric(timeData$f9to10)+1}
      else if(hour==10){timeData$f10to11=as.numeric(timeData$f10to11)+1}
      else if(hour==11){timeData$f11to12=as.numeric(timeData$f11to12)+1}
      else if(hour==12){timeData$f12to13=as.numeric(timeData$f12to13)+1}
      else if(hour==13){timeData$f13to14=as.numeric(timeData$f13to14)+1}
      else if(hour==14){timeData$f14to15=as.numeric(timeData$f14to15)+1}
      else if(hour==15){timeData$f15to16=as.numeric(timeData$f15to16)+1}
      else if(hour==16){timeData$f16to17=as.numeric(timeData$f16to17)+1}
      else if(hour==17){timeData$f17to18=as.numeric(timeData$f17to18)+1}
      else if(hour==18){timeData$f18to19=as.numeric(timeData$f18to19)+1}
      else if(hour==19){timeData$f19to20=as.numeric(timeData$f19to20)+1}
      else if(hour==20){timeData$f20to21=as.numeric(timeData$f20to21)+1}
      else if(hour==21){timeData$f21to22=as.numeric(timeData$f21to22)+1}
      else if(hour==22){timeData$f22to23=as.numeric(timeData$f22to23)+1}
      else if(hour==23){timeData$f23to24=as.numeric(timeData$f23to24)+1}
    }
    q=q+1
  }
  return(timeData)
}

adviceCountOfEmp=function(key,adviceData){
  return(sum(adviceData$owner==key))
}

adviceCountByVisitTypeOfEmp=function(key,adviceData){
  vist_t=list()
  vist_t$other=sum(adviceData$owner==key & adviceData$visit_type=='Other')
  vist_t$field=sum(adviceData$owner==key & adviceData$visit_type=='Field visit')
  vist_t$phone=sum(adviceData$owner==key & adviceData$visit_type=='Phone')
  return(vist_t)
}

getCountOfProductAdviceOfEmp=function(key,adviceData){  
  uni_data=unique(adviceData$owner)
  i=1
  Data=adviceData
  while(i<=length(uni_data)){
    if(uni_data[i]!=key){
      Data=Data[Data$owner!=uni_data[i],]
    }
    i=i+1
  }
  Data <- Data$product
  table(sapply(Data,paste,collapse=""))
  return(table(unlist(Data)))
}

#-----------------------------------------------------------------------------------------
#--------------------------------------Crop Pkg-------------------------------------------
#-----------------------------------------------------------------------------------------

# Support Methods
#-----------------
filterCrop=function(cropData){
  cropData=cropData[cropData$owner!="maheshgrt007@gmail.com",]
  cropData=cropData[cropData$owner!="sagar.75@hotmail.com",]
  cropData$plantation_date= as.Date(cropData$plantation_date)
  cropData$created_at= as.Date(cropData$created_at)
  cropData$count=1
  return(cropData)
}

readCrop=function(){
  return(filterCrop(read.csv("http://ec2-54-187-6-49.us-west-2.compute.amazonaws.com:9002/cropList.csv")))
}

# Group Methods
#-----------------

cropAddedByEmployeeOverall=function(cropData){
  employeewiseCrop=aggregate(cropData$land ~ cropData$owner + cropData$name, data = cropData, sum)
  employeewiseCrop=employeewiseCrop[order(employeewiseCrop$'cropData$owner'),]
  employeewiseCrop=list(split(employeewiseCrop,employeewiseCrop$'cropData$owner'))
  return(employeewiseCrop)
}

cropMonthwisePlantationPattern=function(cropData){
  cropData$temp=substr(cropData$plantation_date, 1, 7)
  monthwiseCrop=aggregate(cropData$land ~ cropData$temp + cropData$name, data = cropData, sum)
  monthwiseCrop=list(split(monthwiseCrop,monthwiseCrop$'cropData$temp'))
  return(monthwiseCrop)
}

cropCount=function(cropData){
  cropData$count=1
  totalVal <- merge(aggregate(cropData$land ~ cropData$name, data = cropData, sum),aggregate(cropData$count ~ cropData$name, data = cropData, sum),by="cropData$name")
  cropData$count=NULL
  cropAcre=list()
  i=1
  while(i<=nrow(totalVal)){
    cropAcre[[i]]=list('crop'=totalVal[i,1],'person'=totalVal[i,3],'land'=totalVal[i,2])
    i=i+1
  }
  return(cropAcre)
}

# Individual Methods
#-----------------




#-----------------------------------------------------------------------------------------
#--------------------------------------Farmer Pkg-----------------------------------------
#-----------------------------------------------------------------------------------------

# Support Methods
#-----------------
filterFarmer=function(farmerData){
  farmerData=farmerData[order(farmerData$created_at),]
  farmerData$times=substr(farmerData$created_at, 12, 19)
  farmerData=farmerData[order(farmerData$owner),]
  farmerData$created_at= as.Date(farmerData$created_at)
  farmerData$totalLand=NULL
  farmerData$count=1
  farmerData=farmerData[farmerData$owner!="maheshgrt007@gmail.com",]
  farmerData=farmerData[farmerData$owner!="sagar.75@hotmail.com",]
  return(farmerData)
}

uniqueOwner_F=function(data){
  return(unique(data$owner))
}

readFarmer=function(){
  return(filterFarmer(read.csv("http://ec2-54-187-6-49.us-west-2.compute.amazonaws.com:9002/farmerList.csv")))
}

# Indivisual Methods
#-----------------
getTimeDetails=function(key,farmerData){
  q=1
  timeData=list('f0to1'=0,'f1to2'=0,'f2to3'=0,'f3to4'=0,'f4to5'=0,'f5to6'=0,'f6to7'=0,'f7to8'=0,'f8to9'=0,'f9to10'=0,'f10to11'=0,'f11to12'=0,'f12to13'=0,'f13to14'=0,'f14to15'=0,'f15to16'=0,'f16to17'=0,'f17to18'=0,'f18to19'=0,'f19to20'=0,'f20to21'=0,'f22to22'=0,'f22to23'=0,'f23to24'=0)
  while(q<nrow(farmerData))
  {
    if(key == farmerData[q,2])
    {
      hour=as.numeric(substr(farmerData[q,]$times, 1, 2))
      if(hour==0){timeData$f0to1=as.numeric(timeData$f0to1)+1}
      else if(hour==1){timeData$f1to2=as.numeric(timeData$f1to2)+1}
      else if(hour==2){timeData$f2to3=as.numeric(timeData$f2to3)+1}
      else if(hour==3){timeData$f3to4=as.numeric(timeData$f3to4)+1}
      else if(hour==4){timeData$f4to5=as.numeric(timeData$f4to5)+1}
      else if(hour==5){timeData$f5to6=as.numeric(timeData$f5to6)+1}
      else if(hour==6){timeData$f6to7=as.numeric(timeData$f6to7)+1}
      else if(hour==7){timeData$f7to8=as.numeric(timeData$f7to8)+1}
      else if(hour==8){timeData$f8to9=as.numeric(timeData$f8to9)+1}
      else if(hour==9){timeData$f9to10=as.numeric(timeData$f9to10)+1}
      else if(hour==10){timeData$f10to11=as.numeric(timeData$f10to11)+1}
      else if(hour==11){timeData$f11to12=as.numeric(timeData$f11to12)+1}
      else if(hour==12){timeData$f12to13=as.numeric(timeData$f12to13)+1}
      else if(hour==13){timeData$f13to14=as.numeric(timeData$f13to14)+1}
      else if(hour==14){timeData$f14to15=as.numeric(timeData$f14to15)+1}
      else if(hour==15){timeData$f15to16=as.numeric(timeData$f15to16)+1}
      else if(hour==16){timeData$f16to17=as.numeric(timeData$f16to17)+1}
      else if(hour==17){timeData$f17to18=as.numeric(timeData$f17to18)+1}
      else if(hour==18){timeData$f18to19=as.numeric(timeData$f18to19)+1}
      else if(hour==19){timeData$f19to20=as.numeric(timeData$f19to20)+1}
      else if(hour==20){timeData$f20to21=as.numeric(timeData$f20to21)+1}
      else if(hour==21){timeData$f21to22=as.numeric(timeData$f21to22)+1}
      else if(hour==22){timeData$f22to23=as.numeric(timeData$f22to23)+1}
      else if(hour==23){timeData$f23to24=as.numeric(timeData$f23to24)+1}
    }
    q=q+1
  }
  return(timeData)
}

getDaysDetails=function(key,farmerData){
  q=1
  daysData=list('Wednesday'=0,'Thursday'=0,'Friday'=0,'Saturday'=0,'Sunday'=0,'Monday'=0,'Tuesday'=0)
  while(q<nrow(farmerData))
  {
    if(key == farmerData[q,2])
    {
      day=weekdays(farmerData[q,]$created_at)
      if(day=="Wednesday"){daysData$Wednesday=as.numeric(daysData$Wednesday)+1}
      else if(day=="Thursday"){daysData$Thursday=as.numeric(daysData$Thursday)+1}
      else if(day=="Friday"){daysData$Friday=as.numeric(daysData$Friday)+1}
      else if(day=="Saturday"){daysData$Saturday=as.numeric(daysData$Saturday)+1}
      else if(day=="Sunday"){daysData$Sunday=as.numeric(daysData$Sunday)+1}
      else if(day=="Monday"){daysData$Monday=as.numeric(daysData$Monday)+1}
      else if(day=="Tuesday"){daysData$Tuesday=as.numeric(daysData$Tuesday)+1}
    }
    q=q+1
  }
  return(daysData)
}

farmerList=function(key,farmerData){
  i=j=1
  data=list()
  while(i!=nrow(farmerData))
  {
    if(key == farmerData[i,2])
    {
      data[[j]]=list('name'= farmerData[i,1],'date'= toString(farmerData[i,3]), 'latitude'=  farmerData[i,4], 'longitude'= farmerData[i,5])
      j=j+1
    }
    i=i+1
  }
  return(data)
}

farmerCountByEmp=function(key,data){
  i=1
  j=0
  while(i!=nrow(data))
  {
    if(key == data[i,2])
      j=j+1
    i=i+1
  }
  return(j)
}

# Group Methods
#-----------------


farmerAddDaysPatternOfEmployee=function(farmerData){
  dayAnalysis=list()
  a=1
  uni=uniqueOwner_F(farmerData)
  for(i in uni){
    dayAnalysis[[a]]=list('owner'=i, 'dayAnalysis' = getDaysDetails(i,farmerData))
    a=a+1
  }
  return(dayAnalysis)
}

farmerAddTimePatternOfEmployee=function(farmerData){
  timeAnalysis=list()
  a=1
  uni=uniqueOwner_F(farmerData)
  for(i in uni){
    timeAnalysis[[a]]=list('owner'=i, 'timeAnalysis' = getTimeDetails(i,farmerData))
    a=a+1
  }
  return(timeAnalysis)
}

farmerListByEmployee = function(farmerData){
  data=list()
  a=1
  uni=uniqueOwner_F(farmerData)
  for(i in uni){
    data[[a]]=list('owner'=i, 'farmerList' = farmerList(i,farmerData))
    a=a+1
  }
  return(data)
}

#--------------------------------------------------------------------------------


# Overall Stat Div
overallStats=function(param){
	farmer=readFarmer()
	advice=readAdvice()
	data=list('Farmer'=list('TotalCount'=entityCount(farmer),'TodaysCount'=todaysEntityCount(farmer),'CurrentMonthCount'=currentMonthEntityCount(farmer),'LastWeekReport'=list(entityCountLastWeek(farmer))),
            'Advice'=list('TotalCount'=entityCount(advice),'TodaysCount'=todaysEntityCount(advice),'CurrentMonthCount'=currentMonthEntityCount(advice),'LastWeekReport'=list(entityCountLastWeek(advice))),
            'Product'=list('TodaysCount'=countProductAdviceByDate(advice,Sys.Date()),'LastWeekReport'=list(countLastWeekProductAdvice(advice)))#,'CurrentMonthCount'=countCurrentMonthProductAdvice(advice))
    )
	return(create_json(data))
}

# Overall Employee Stat
overallEmployeeStat=function(param){
  farmer=readFarmer()
  advice=readAdvice()
  data=list()
  j=1
  uniqueEmp=uniqueOwner_F(farmer)
  for(i in uniqueEmp){
    data[[j]]=list('Name'=i,'FarmerAdded'=farmerCountByEmp(i,farmer),'CurrentMonthAdviceGiven'=countCurrentMonthVisitByEmp(i,advice),'TodaysVisits'=countTodaysVisitByEmp(i,advice))
    j=j+1
  }
  return(create_json(data))
}


# Crop Stat
cropStat=function(param){
  crop=readCrop()
  return(create_json(cropCount(crop)))
}

#Employee Stat
employeeStat=function(param){
  library(rjson)
  param <- fromJSON(param)
  key = param$first
  farmer=readFarmer()
  advice=readAdvice()
  data=list('Name'=key,
            'FarmerCount'=farmerCountByEmp(key,farmer),
            'TodaysVisit'=countTodaysVisitByEmp(key,advice),
            'CurrentMonthAdviceCount'=countCurrentMonthVisitByEmp(key,advice),
            # 'CurrentMonthProductAdviceCount'=countCurrentMonthProductAdvice(data[data$owner==key,]),
            'FarmerList'=farmerList(key,farmer)
            )
   return(create_json(data))
}

#Farmer Profile
farmerStat=function(param){
  library(rjson)
  param <- fromJSON(param)
  key = param$first
  farmer=readFarmer()
  advice=readAdvice()
  crop=readCrop()

  return(create_json(param))
}

