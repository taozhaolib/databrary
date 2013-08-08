// clean dbjs namespace
var dbjs = {};

// setup site features
(function () {
    var isTouch = 'ontouchstart' in document.documentElement;
    var siteFooter = $('#site_footer');

    dbjs.stickyFooter = function() {
        // chrome has a major bug. renders event before dom finished loading.
        // find another way. (programmatic fixed-width css version, likely.)
        if($(window).innerHeight() > siteFooter.offset().top + siteFooter.height()) {
            siteFooter.addClass('sticky');
        } else {
            siteFooter.removeClass('sticky');
        }
    }

    dbjs.toggleFold = function(fold, folder, folded) {
        var folders = $(fold + ' ' + folder),
            foldeds = $(fold + ' ' + folded);

        folders.click(function(e){
            $(this).next(folded).slideToggle();
            $(this).toggleClass('open');
        });

        folders.prepend('<span class="arrow"></span>')
        foldeds.toggle();
    }
}());

// event bindings
// ...

// initialization
$(document).ready(function () {
    dbjs.toggleFold('.question', 'h2', 'div');








    // refactor AND switch to modals!!!
    $('.drop_down .drop_toggle:not(".nil") a').click(function (e) {
        e.preventDefault();
    });
    $('.drop_down .drop_toggle').click(function (e) {
        var dropDown = $(e.currentTarget).parent();

        dropDown.toggleClass('dropped');

        e.preventDefault();
    });

    // animation helpers REFACTOR
    var waitForFinalEvent = (function () {
        var timers = {};
        
        return function (callback, ms, uniqueId) {
            if (!uniqueId) {
                uniqueId = "Don't call this twice without a uniqueId";
            }
            
            if (timers[uniqueId]) {
                clearTimeout (timers[uniqueId]);
            }
            
            timers[uniqueId] = setTimeout(callback, ms);
        };
    })();
});
