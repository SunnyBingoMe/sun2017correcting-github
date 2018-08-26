
Shiny.addCustomMessageHandler("testmessage",
    function(message) {
        //alert("The b variable is " + message.b); // for list-jason msg
        alert(message);
    }
);

//(function(){
//    $("#nextAccidentButton").click()
//    setTimeout(arguments.callee, 1000);
//})();
