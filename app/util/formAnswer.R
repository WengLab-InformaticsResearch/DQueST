formAnswer = function(input){
  answer = NULL
  answer$exist = input$radio_qa
  answer$status = input$status_qa
  if(!is.numeric(input$value_qa)){
    answer$value = -1
    showNotification("Your enter for value is not numeric, change to -1.")
  }else{
    answer$value = input$value_qa
  }
  if(!is.numeric(answer$time)){
    answer$time = -1
  }else{
    answer$time = convert2day(input$time_qa,input$time_unit_qa)
    showNotification("Your enter for value is not numeric, change to -1.")
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