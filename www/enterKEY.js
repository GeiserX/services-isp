$(document).keyup(function(event) {
    if (( $("#numClienteSinEXT").is(":focus") || $("#nombreClienteSinEXT").is(":focus") ) && (event.keyCode == 13)) {
        $("#searchSinEXT").click();
    }
});

$(document).keyup(function(event) {
    if ($("#numCliente").is(":focus") && (event.keyCode == 13)) {
        $("#search").click();
    }
});

$(document).keyup(function(event) {
    if (( $("#numClienteSinEXT2").is(":focus") || $("#nombreClienteSinEXT2").is(":focus") ) && (event.keyCode == 13)) {
        $("#searchSinEXT2").click();
    }
});

$(document).keyup(function(event) {
    if ($("#numCliente2").is(":focus") && (event.keyCode == 13)) {
        $("#search2").click();
    }
});