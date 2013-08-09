// clean dbjs namespace
var dbjs = {};

// setup site features
(function () {
    var isTouch = 'ontouchstart' in document.documentElement;

    dbjs.toggleFold = function(fold, folder, folded) {
        var folders = $(fold + ' ' + folder),
            foldeds = $(fold + ' ' + folded);

        folders.click(function(e){
            $(this).next(folded).slideToggle();
            $(this).toggleClass('unfolded');
        });

        folders.prepend('<span class="arrow"></span>')
        foldeds.toggle();
    }
}());

// event bindings
// ...

// initialization
$(document).ready(function () {
    // registration should only appear on the pages it's need. I've added it to-do.
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
