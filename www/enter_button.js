$(document).keyup(function(event) {
    if ($("#score_count").is(":focus") && (event.key == "Enter")) {
        $("#score").click();
    }
});