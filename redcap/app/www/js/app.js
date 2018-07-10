Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.onInputChange(variableName, null);
    });