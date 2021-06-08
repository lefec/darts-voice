$(document).keyup(function(event) {
    if ($("#score_count").is(":focus") && (event.key == "Enter")) {
        $("#score").click();
    }
});


$(document).keyup(function(event) {
    if ($("#score_count").is(":focus") && (event.key == "r")) {
        $("#score_rest").click();
    }
});