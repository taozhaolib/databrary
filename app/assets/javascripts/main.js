// jQuery extensions
$.fn.exists = function () {
    return this.length !== 0;
};

$.fn.escape = function () {
    return this
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
};

$.fn.unescape = function () {
    return this
        .replace(/&amp;/g, "&")
        .replace(/&lt;/g, "<")
        .replace(/&gt;/g, ">")
        .replace(/&quot;/g, "\"")
        .replace(/&#039;/g, "'");
};

// clean dbjs namespace
var dbjs = {};

dbjs.vars = {
    speedFast: 150,
    speedNorm: 250,
    speedSlow: 350,
    speedTest: 1000
};

/**
 * Creates tabset with content panes
 * @param tabset    containing element
 * @param tab        class of tabs
 * @param body        class of content panes
 */
dbjs.tabs = function (tabset, tab, body) {
    var $tabset = $(tabset),
        $tabs = $tabset.find(tab),
        $bodies = $tabset.find(body),
        $tabs_el = $('<ul class="tabs cf"></ul>'),
        $body_el = $('<div class="views cf"></div>');

    // rebuild dom
    $tabset.prepend($tabs_el);
    $tabset.append($body_el);

    $tabs.each(function () {
        var $this = $(this),
            id = $this.parent().attr('id');

        $this.appendTo($tabs_el).replaceWith($('<li class="tab"><a href="#' + id + '">' + this.innerHTML + '</a></li>'));
    });

    $bodies.each(function () {
        $(this).appendTo($body_el).addClass('rolled').slideUp(0);
    });

    // tab clicking
    $tabs = $('ul.tabs ' + tab);
    $bodies = $('.views ' + body);

    var $tablinks = $tabs.find('a'),
        $currentBody = $bodies.first();

    var clickTab = function (tab) {
        var $this = $(tab),
            id = $this.attr('href'),
            $body = $(id);

        if ($this.parent().hasClass('current')) {
            $currentBody.fadeOut(150, function () {
                $currentBody.fadeIn(150);
            });

            return;
        }

        $currentBody.slideUp(250, function () {
            $body.slideDown(250);
        });

        $tabs.removeClass('current');
        $this.parent().addClass('current');

        $currentBody = $body;
    };

    $tablinks.click(function (e) {
        clickTab(this);

        return false;
    });

    clickTab($tablinks.first());
};

/**
 * Creates rolldown modal content.
 * @param clicker    click toggle
 * @param toggle    toggled area
 * @param now        display modal now, or on click
 */
dbjs.modal = function (clicker, toggle, now) {
    if (typeof(now) === 'undefined') now = false;

    var $clicker = $(clicker),
        $toggle = $(toggle),
        modals = '.modal',
        back = '#modal_back',
        $back,
        speed = 250;

    // modal background
    if (!$(back).exists()) {
        $('body').append('<div id="modal_back"></div>');
        $back = $(back);

        $back.click(function (e) {
            $(modals).slideUp(speed);
            $(this).fadeOut(speed);
        });
    }

    // clicker
    var click = function () {
        $toggle.slideDown(speed);
        $back.fadeIn(speed);
        $("html, body").animate({ scrollTop: 0 }, speed);
    };

    $clicker.click(function (e) {
        click();
        return false;
    });

    if (now === true) {
        click();
    }
};

/**
 * Creates rolldown modal content filled by ajax.
 * @param clicker    click toggle
 * @param url        ajax retrieved content
 * @param now        load content now, or on click
 */
dbjs.ajaxModal = function (clicker, url, now) {
    if (typeof(now) === 'undefined') now = false;

    var $clicker = $(clicker),
        $toggle, toggle;

    if (!$clicker.exists())
        return;

    var setup = function (url) {
        $clicker.off('click');

        $.get(url, function (data) {
            var $data = $(data),
                $script = $data.find('script');

            $toggle = $data.find('.modal');
            toggle = $clicker.attr('data-target');

            $toggle.attr('id', toggle).appendTo($('#site_body'));

            toggle = '#' + toggle;

            if ($script.exists())
                $script.appendTo($('body'));

            if (now === true) {
                dbjs.modal(clicker, toggle);
            } else {
                $clicker.off('click.ajaxModal');
                dbjs.modal(clicker, toggle, true);
            }

        }, 'html');

        $clicker.on('click');
    }

    if (now === true) {
        setup(url);
    } else {
        $clicker.on('click.ajaxModal', function (e) {
            setup(url);
        });
    }
};

/**
 * Creates folding content.
 * @param region    container element
 * @param clicker    click toggle, child of fold
 * @param toggle    toggled area, child of fold
 */
dbjs.fold = function (region, clicker, toggle) {
    var $clicker = $(region + ' ' + clicker),
        $toggles = $(region + ' ' + toggle);

    $clicker.click(function (e) {
        $(this).next(toggle).slideToggle();
        $(this).toggleClass('unfolded');
    });

    $clicker.prepend('<span class="arrow"></span>');
    $toggles.toggle();
};

/**
 * Creates a compact menu for small browser windows
 * @param menu        region to resize
 * @param position    where to position menu link
 */
dbjs.sideMenu = function (menu, position) {
    var $menu = $(menu),
        $position = $(position),
        $origin = $menu.parent(),
        $menuHeader = $menu.find('h1'),
        $menuLink = $('<button class="hide"></button>'),
        toggle = false;

    // build menu elements
    $menuLink.prependTo($menu);

    // hide and show based on browser size and zoom
    var reducer = function () {
        if (toggle == false && $menu.css('z-index') == 1) {
            $menu.addClass('reduced').prependTo($position);
            $menuHeader.remove();
            $menuLink.removeClass('hide');
            toggle = true;
        } else if (toggle == true && $menu.css('z-index') == 'auto') {
            $menu.removeClass('reduced').prependTo($origin);
            $menuHeader.prependTo($menu);
            $menuLink.addClass('hide');
            toggle = false;
        }
    };

    $(window).resize(function (e) {
        reducer();
    });

    reducer();

    // make menu clickable
    $menuLink.click(function (e) {
        $menu.toggleClass('dropped');
    });

    $menu.click(function (e) {
        e.stopPropagation();
    });

    $(document).click(function (e) {
        $menu.removeClass('dropped');
    });
};

/**
 * Keeps footer at the bottom of the window when the content is shorter than the window
 * @param footer    element made sticky
 * @param above    content to monitor for height
 */
dbjs.stickyFooter = function (footer, above) {
    var $above = $(above),
        aboveW = $above.width(),
        aboveH = $above.height();

    // monitor window
    var resize = function (footer, above) {
        var windowHeight = $(window).height(),
            $footer = $(footer),
            $above = $(above),
            footerOffset = $footer.outerHeight() + $above.outerHeight(true) + $above.position().top;

        if (windowHeight > footerOffset)
            $footer.addClass('fixed');
        else
            $footer.removeClass('fixed');
    };

    $(window).resize(function (e) {
        resize(footer, above);
    });

    // monitor content
    var checkResize = function () {
        var w = $above.width(),
            h = $above.height();

        if (w != aboveW || h != aboveH) {
            aboveW = w;
            aboveH = h;

            return true;
        }

        return false;
    };

    if (screen.height > aboveH * (2 / 3)) {
        // skip the interval check if the content is sufficiently long
        // this estimate can be greatly improved by comparing screen and browser width
        var contentCheck = setInterval(function () {
            if (checkResize())
                resize(footer, above);
        }, 50);
    }

    resize(footer, above);
};

/**
 * Fades the bottom of longer content based on shorter content size
 * @param container wrapper for fader and faded element
 * @param faded    element faded to fit
 * @param fader    element measured for height
 */
dbjs.fadeOff = function (container, faded, fader) {
    var $container = $(container),
        fade = '<div class="fade"></div>';

    // could replace this with img.load().each(if complete, load...)
    // but which looks better to the user?
    $(window).load(function () {
        $container.each(function () {
            var $this = $(this),
                $faded = $this.find(faded),
                $fader = $this.find(fader);

            $faded.height($fader.height());
            $faded.append($(fade));
        });
    });
};

/**
 * Uses a set of togglers to hide all but the active element and the inverse set of togglers
 * @param toggler    elements used to toggle
 * @param toggled    elements toggled by togglers
 */
dbjs.simpleToggle = function (toggler, toggled) {
    var $togglers = $(toggler),
        $toggleds = $(toggled),
        $active = $togglers.filter('.active');

    var clicker = function (toggler) {
        var $toggler = $(toggler),
            $toggled = $($toggler.attr('href'));

        $toggler.fadeOut(dbjs.vars.speedNorm, function () {
            $togglers.not($toggler).fadeIn(dbjs.vars.speedNorm);
        });

        $toggleds.not($toggled).slideUp(dbjs.vars.speedNorm, function () {
            $toggled.slideDown(dbjs.vars.speedNorm);
        });
    };

    $togglers.click(function (e) {
        e.stopPropagation();

        clicker(this);
    });

    if ($active.exists())
        clicker($active);
};

dbjs.tooltip = function (region, source, direction, event, now) {
    console.log(region, source, direction, event, now);

    var $regions = $(region),
        directionArray = ['ordeal', 'diagonal', 'n', 'e', 's', 'w', 'ne', 'se', 'sw', 'nw'],
        sourceArray = ['auto', 'alt', 'title', 'data-tip', 'data-source'],
        eventArray = ['hover', 'click', 'toggle', 'focus'];

    console.log($regions, directionArray, sourceArray, eventArray);

    // defaults
    if (!$.inArray(direction, directionArray)) direction = 'ordeal';
    if (!$.inArray(source, sourceArray)) source = 'auto';
    if (!$.inArray(event, eventArray)) event = 'hover';
    if (typeof(now) === 'undefined') now = false;

    console.log(region, source, direction, event, now);

    // source functions
    var prepareSource = function ($region, source) {
        console.log($region, source);

        var tipID = 'tip' + new Date().getTime(),
            $tip = $('<aside id="' + tipID + '" class="tip"></aside>');

        console.log(tipID, $tip);

        // pick 'auto' source
        if (source == 'auto') {
            if ($region.attr('data-source'))
                source = 'data-source';
            else if ($region.attr('data-tip'))
                source = 'data-tip';
            else if ($region.attr('title'))
                source = 'title';
            else if ($region.attr('alt'))
                source = 'alt';
            else // no source? that's dumb.
                return false;
        }

        console.log(source);

        // grab the content
        if (source == 'data-source')
            $tip.html($($region.attr('data-source')));
        else
            $tip.html($region.attr(source));

        console.log($tip);

        // place the content
        if (!now)
            $tip.hide();

        $region.append($tip);

        // clean up
        $region.attr('data-source', '#' + tipID)
            .removeAttr('alt')
            .removeAttr('title')
            .removeAttr('data-tip');

        console.log($region);

        return true;
    };

    var setSource = function ($region, $source) {
        return $region.attr('data-tip', $source.escape());
    };

    var getSource = function ($region) {
        return $region.attr('data-tip').unescape();
    };

    // direction functions
    var getDirection = function ($region, direction) {
        // simple exit for pre-defined
        if (direction.length <= 2)
            return direction;

        // setup for 'ordeal' and 'diagonal'
        var $window = $(window),
            wWidth = $window.width(),
            wHeight = $window.height(),
            rWidth = $region.width(),
            rHeight = $region.height();

        var compass = {
                west: $region.position().left,
                east: wWidth - $region.position().left - rWidth,
                north: $region.position().top,
                south: wHeight - $region.position().top - rHeight
            },
            compassSort = [];

        for (var dir in compass)
            compassSort.push([dir, compass[dir]]);

        compassSort.sort(function (a, b) {
            return a[1] - b[1]
        });

        // ordeal or diagnoal exit...
        var dirClass = compassSort.shift()[0];

        if (direction == 'ordeal')
            return dirClass;

        for (var pair in compassSort) {
            if (dirClass == 'n' || dirClass == 's') {
                if (pair[0] == 'e' || pair[0] == 'w') {
                    dirClass = dirClass + pair[0];
                    break;
                }
            } else {
                if (pair[0] == 'n' || pair[0] == 's') {
                    dirClass = pair[0] + dirClass;
                    break;
                }
            }
        }

        return diag;
    };

    // display functions
    var showTip = function ($region) {
        $region
            .find($region.attr('data-source'))
            .addClass(getDirection($region, direction))
            .fadeIn(dbjs.vars.speedNorm);
    };

    var hideTip = function ($region) {
        $region
            .find($region.attr('data-source'))
            .removeClass(directionArray.join(' '))
            .fadeOut(dbjs.vars.speedNorm);
    };

    console.log('');

    // register the events
    $regions.each(function (i, e) {
        var $this = $(this);

        console.log($this);

        if (prepareSource($this, source)) {
            switch (event) {
                case 'hover':
                    $this.hover(function (e) {
                        showTip($this);
                    }, function (e) {
                        hideTip($this);
                    });

                    break;

                case 'click':
                    $this.click(function (e) {
                        showTip($this);
                    });

                    $(document).click(function (e) {
                        hideTip($this);
                    });

                    break;

                case 'toggle':
                    $this.toggle(function (e) {
                        showTip($this);
                    }, function (e) {
                        hideTip($this);
                    });

                    break;

                case 'focus':
                    $this.focusin(function (e) {
                        showTip($this);
                    });

                    $this.focusout(function (e) {
                        hideTip($this);
                    });

                    break;
            }
        }

        console.log('');
    });
};

// initialization
$(document).ready(function () {
    // TODO: event registration should only appear on the pages it's need. In the works.

    // all pages
    dbjs.stickyFooter('#site_footer', '#site_body');

    // all static pages
    dbjs.sideMenu('.sidebar_menu', 'main > article > h1:first-child');

    // when logged out
    dbjs.ajaxModal('#modal_login_link', '/ajax/modal/login', true);
    // when logged in
    dbjs.ajaxModal('#modal_profile_link', '/ajax/modal/profile', true);

    // faq
    dbjs.fold('.question', 'h2', 'div');

    // study (none other currently)
    dbjs.tabs('.tabset', '.tab', '.view');

    // study list
    dbjs.fadeOff('.study_roll a', '.body', '.thumb')
});
