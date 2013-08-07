$(document).ready(function () {
    // clean dbjs namespace
    var dbjs = {};

    // setup site features
    (function dbjs_setup() {
        var isTouch = 'ontouchstart' in document.documentElement;

        var siteFooter = $('#site_footer');

        dbjs.stickyFooter = function() {
            console.log($(window).innerHeight() +"  "+ siteFooter.offset().top +"  "+ siteFooter.height());
            if($(window).innerHeight() > siteFooter.offset().top + siteFooter.height()) {
                siteFooter.addClass('sticky');
            } else {
                siteFooter.removeClass('sticky');
            }
        }
    }());

    // refactor or switch to modals!!!
    $('.drop_down .drop_toggle:not(".nil") a').click(function (e) {
        e.preventDefault();
    });
    $('.drop_down .drop_toggle').click(function (e) {
        var dropDown = $(e.currentTarget).parent();

        dropDown.toggleClass('dropped');

        e.preventDefault();
    });

    // bindings
    //

    // animation helpers
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

    // on page load
    //
});