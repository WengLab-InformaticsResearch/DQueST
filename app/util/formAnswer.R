formAnswer = function(input){
  answer = NULL
  answer$exist = input$radio_qa
  answer$status = input$status_qa
  answer$value = input$value_qa
  answer$time = convert2day(input$time_qa,input$time_unit_qa)
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