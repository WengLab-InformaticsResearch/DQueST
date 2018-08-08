formAnswer = function(input){
  answer = NULL
  answer$exist = input$radio_qa
  answer$status = input$status_qa
  if(length(input$value_qa) > 0){
    if(!is.numeric(input$value_qa)){
      answer$value = -1
      showNotification("Your enter for value is not numeric, change to -1.")
    }else{
      answer$value = input$value_qa
    }
  }
  if(length(input$time_qa) > 0){
    if(!is.numeric(input$time_qa)){
      showNotification("Your enter for time1 is not numeric, change to -1.")
    }else{
      answer$time$time1 = convert2day(input$time_qa,input$time_unit_qa)
    }
  }
  if(length(input$time_qa2) > 0){
    if(!is.numeric(input$time_qa2)){
      showNotification("Your enter for time2 is not numeric, change to -1.")
    }else{
      answer$time$time2 = convert2day(input$time_qa2,input$time_unit_qa2)
    }
  }
  
  return(answer)
}

convert2day = function(time, unit) {
  days = NULL
  if(!is.null(time)){
    days = switch(
      unit,
      "day" = time * 1,
      "week" = time * 7,
      "month" = time * 31,
      "year" =  time * 365,
      "hour" = time * 1/24,
      NULL
    )
  }
  return(days)
}