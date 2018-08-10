formAnswer = function(input){
  answer = NULL
  answer$exist = input$radio_qa
  answer$status = input$status_qa
  answer$time = NULL
  if(length(input$value_qa) > 0){
    if(input$value_qa < 0){
      showNotification("Your enter for value is < 0, are you sure?")
    }else{
      answer$value = input$value_qa
    }
  }
  if(!is.null(input$radio_qa)){
    if(input$radio_qa == "YES" & !is.null(input$time_qa)){
      if(input$time_qa < 0){
        showNotification("Your enter for time1 is < 0, change to 0.")
        answer$time$time1 = 0
      }else{
        answer$time$time1 = convert2day(input$time_qa,input$time_unit_qa)
      }
      answer$time$time1 = -answer$time$time1 # max
    }
    if(input$radio_qa == "YES" & !is.null(input$time_qa2)){
      answer$time$time2 = convert2day(input$time_qa2,input$time_unit_qa2)
      if(answer$time$time2 < -answer$time$time1){
        answer$time$time2 = answer$time$time1
        showNotification("Your enter for time2 is < time, change to time1.")
      }
      answer$time$time2 = -answer$time$time2 # min
    }
  }
  
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
      "hours" = time * 1/24,
      NULL
    )
  }
  return(days)
}