// jQuery extensions
$.fn.exists = function () {
	return this.length !== 0;
}

// clean dbjs namespace
var dbjs = dbjs || {};

function extend(ns, ns_string) {
	var parts = ns_string.split('.'),
		parent = ns,
		pl, i;
	if (parts[0] == "myApp") {
		parts = parts.slice(1);
	}
	pl = parts.length;
	for (i = 0; i < pl; i++) {
		//create a property if it doesnt exist
		if (typeof parent[parts[i]] == 'undefined') {
			parent[parts[i]] = {};
		}
		parent = parent[parts[i]];
	}
	return parent;
}

// setup site features
(function () {
	var isTouch = 'ontouchstart' in document.documentElement;

	var $window = $(window),
		$document = $(document),
		$body = $('body');

	dbjs.toggleFold = function (fold, folder, folded) {
		var $folders = $(fold + ' ' + folder),
			$foldeds = $(fold + ' ' + folded);

		$folders.click(function (e) {
			$(this).next(folded).slideToggle();
			$(this).toggleClass('unfolded');
		});

		$folders.prepend('<span class="arrow"></span>')
		$foldeds.toggle();
	}

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

	dbjs.modalLink = function (links, targets) {
		var $links = $(links),
			back = '#modal_back',
			$back;

		if (!$(back).exists()) {
			$("body").append('<div id="modal_back"></div>');
			$back = $(back);
		}

		$back.click(function (e) {
			$(targets).fadeOut();
			$(this).fadeOut();
		});

		$links.click(function (e) {
			var $target = $('#' + $(this).attr('data-target'));

			$target.fadeIn();
			$back.fadeIn();

			return false;
		});
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

			if($this.parent().hasClass('current')) {
				$currentView.fadeOut(150, function() {
					$currentView.fadeIn(150);
				});

				return;
			}

			$currentView.slideUp(250, function() {
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
	dbjs.toggleFold('.question', 'h2', 'div');
	dbjs.menuReduce('.sidebar_menu', 'main > article > h1:first-child');
	dbjs.footerFixer('#site_footer', '#site_body');
//	dbjs.modalLink('.modal_link', '.modal');
	dbjs.tabs('.tabset', '.tab', '.view');
});
