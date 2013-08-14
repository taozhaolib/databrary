$(document).ready(function() {
	var isTouch = 'ontouchstart' in document.documentElement;
	
	$('.drop_down .drop_toggle:not(".nil") a').click(function(e) {
		e.preventDefault();
	});
	$('.drop_down .drop_toggle').click(function(e) {
		var dropDown = $(e.currentTarget).parent();
		
		dropDown.toggleClass('dropped');
		
		e.preventDefault();
	});
});