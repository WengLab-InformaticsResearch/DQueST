formAnswer = function(input,session){
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
      "days" = time * 1,
      "weeks" = time * 7,
      "months" = time * 31,
      "years" =  time * 365,
      NULL
    )
  }
  return(days)
}