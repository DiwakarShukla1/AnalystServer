

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
  data=head(data,7)
  return(data)
}