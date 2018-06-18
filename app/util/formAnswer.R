formAnswer = function(input,session){
  answer = NULL
  answer$exist = input$radio
  answer$status = input$status
  answer$value = input$value
  answer$time = convert2day(input$time,input$time_unit)
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
      "years" = time * 365,
      NULL
    )
  }
  return(days)
}