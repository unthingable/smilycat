$(document).ready(function() {
    var s = $("#navigation");
    var pos = s.position();
    $(window).scroll(function() {
	var windowpos = $(window).scrollTop();
	// s.html("Distance from top:" + pos.top + "<br />Scroll position: " + windowpos);
	if (windowpos >= pos.top) {
	    // s.removeClass("unstick");
	    s.addClass("stick");
	} else {
	    s.removeClass("stick");
	    // s.addClass("unstick");
	}
    });
});
