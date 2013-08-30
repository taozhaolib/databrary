// jQuery extensions
$.fn.exists = function () {
	return this.length !== 0;
}

// clean dbjs namespace
var dbjs = dbjs || {};

/**
 * Creates folding content.
 * @param region container element
 * @param clicker click toggle, child of fold
 * @param toggle toggled area, child of fold
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
 * Creates rolldown modal content.
 * @param clicker click toggle
 * @param toggle toggled area
 */
dbjs.modal = function (clicker, toggle, now) {
	if (typeof(now) === 'undefined') now = false;

	var $clicker = $(clicker),
		$toggle = $(toggle),
		modals = '.modal',
		back = '#modal_back',
		$back;

	// modal background
	if (!$(back).exists()) {
		$('body').append('<div id="modal_back"></div>');
		$back = $(back);

		$back.click(function (e) {
			$(modals).fadeOut(250);
			$(this).fadeOut(250);
		});
	}

	// clicker
	var click = function () {
		$toggle.fadeIn();
		$back.fadeIn();
	};

	$clicker.click(function (e) {
		click();
		return false;
	});

	if (now === true) {
		click();
	}
};

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

			toggle = '#'+toggle;

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

// setup site features
(function () {
	dbjs.menuReduce = function (menu, position) {
		var $menu = $(menu),
			$position = $(position),
			$origin = $menu.parent(),
			$menuHeader = $menu.find('h1'),
			$menuLink = $('<button class="hide"></button>'),
			toggle = false;

		function menuReducer() {
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
		}

		$(window).resize(function (e) {
			menuReducer();
		});

		menuReducer();

		$menuLink.prependTo($menu);
		$menuLink.click(function (e) {
			$menu.toggleClass('dropped');
		});
		$menu.click(function (e) {
			e.stopPropagation();
		});
		$(document).click(function (e) {
			$menu.removeClass('dropped');
		});
	}

	dbjs.footerFixer = function (element, above) {
		var resizer = function (element, above) {
			var windowHeight = $(window).height(),
				$footer = $(element),
				$above = $(above),
				footerOffset = $footer.outerHeight() + $above.outerHeight(true) + $above.position().top;

			if (windowHeight > footerOffset)
				$footer.addClass('fixed');
			else
				$footer.removeClass('fixed');
		}

		$(window).resize(function (e) {
			resizer(element, above);
		});

		var $above = $(above),
			aboveW = $above.width(),
			aboveH = $above.height();

		var checkResize = function () {
			var w = $above.width(),
				h = $above.height();

			if (w != aboveW || h != aboveH) {
				aboveW = w;
				aboveH = h;

				return true;
			}

			return false;
		}

		var contentCheck = setInterval(function () {
			if (checkResize())
				resizer(element, above);
		}, 50);

		resizer(element, above);
	}

	dbjs.tabs = function (tabset, tab, view) {
		var $tabset = $(tabset),
			$tabs = $tabset.find(tab),
			$views = $tabset.find(view),
			$tabs_el = $('<ul class="tabs cf"></ul>'),
			$view_el = $('<div class="views cf"></div>');

		// reorganize
		$tabset.prepend($tabs_el);
		$tabset.append($view_el);

		$tabs.each(function () {
			var $this = $(this),
				id = $this.parent().attr('id');

			$this.appendTo($tabs_el).replaceWith($('<li class="tab"><a href="#' + id + '">' + this.innerHTML + '</a></li>'));
		});

		$views.each(function () {
			$(this).appendTo($view_el).addClass('rolled').slideUp(0);
		});

		// click
		$tabs = $('ul.tabs ' + tab);
		$views = $('.views ' + view);

		var $tablinks = $tabs.find('a'),
			$currentView = $views.first();

		var clickTab = function (tab) {
			var $this = $(tab),
				id = $this.attr('href'),
				$view = $(id),
				viewHeight = $view[0].scrollHeight;

			if ($this.parent().hasClass('current')) {
				$currentView.fadeOut(150, function () {
					$currentView.fadeIn(150);
				});

				return;
			}

			$currentView.slideUp(250, function () {
				$view.slideDown(250);
			});

			$tabs.removeClass('current');
			$this.parent().addClass('current');

			$currentView = $view;
		}

		$tablinks.click(function (e) {
			clickTab(this);

			return false;
		});

		clickTab($tablinks.first());
	}
}());

// initialization
$(document).ready(function () {
	// event registration should only appear on the pages it's need.
	dbjs.fold('.question', 'h2', 'div');
	dbjs.menuReduce('.sidebar_menu', 'main > article > h1:first-child');
	dbjs.footerFixer('#site_footer', '#site_body');
	dbjs.ajaxModal('#modal_login_link', '/ajax/modal/login', true);
	dbjs.tabs('.tabset', '.tab', '.view');
});
