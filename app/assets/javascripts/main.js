// jQuery extensions
$.fn.exists = function () {
	return this.length !== 0;
}

// clean dbjs namespace
var dbjs = dbjs || {};

/**
 * Creates tabset with content panes
 * @param tabset 	containing element
 * @param tab		class of tabs
 * @param body		class of content panes
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
 * @param clicker 	click toggle
 * @param toggle 	toggled area
 * @param now 		display modal now, or on click
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
 * @param clicker 	click toggle
 * @param url 		ajax retrieved content
 * @param now 		load content now, or on click
 */
dbjs.ajaxModal = function (clicker, url, now) {
	if (typeof(now) === 'undefined') now = false;

	var $clicker = $(clicker),
		$toggle, toggle;

	var setup = function (url) {
		$clicker.off('click');

		$.get(url, function (data) {
			console.log($(data));

			$toggle = $(data).find('.modal');
			toggle = $clicker.attr('data-target');

			$toggle.attr('id', toggle).appendTo($('body'));

			toggle = '#' + toggle;

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
 * @param region 	container element
 * @param clicker 	click toggle, child of fold
 * @param toggle 	toggled area, child of fold
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
 * @param menu		region to resize
 * @param position	where to position menu link
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
 * @param footer 	element made sticky
 * @param above 	content to monitor for height
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

	var contentCheck = setInterval(function () {
		if (checkResize())
			resize(footer, above);
	}, 50);

	resize(footer, above);
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
	//dbjs.ajaxModal('#modal_profile_link', '/ajax/modal/login', true);

	// faq
	dbjs.fold('.question', 'h2', 'div');

	// study (none other currently)
	dbjs.tabs('.tabset', '.tab', '.view');
});
