

// clean dbjs namespace
var dbjs = {};

dbjs.vars = {
	speedFast: 150,
	speedNorm: 250,
	speedSlow: 350,
	speedTest: 1000
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

		$back.click(function () {
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

	$clicker.click(function () {
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
				$script = $data.filter('script');

			$toggle = $data.find('.modal');
			toggle = $clicker.attr('data-target');

			$toggle.attr('id', toggle).appendTo($('#site_content'));

			toggle = '#' + toggle;

			if ($script.exists())
				$script.appendTo($('body'));

			if (now === true) {
				dbjs.modal(clicker, toggle, false);
			} else {
				$clicker.off('click.ajaxModal');
				dbjs.modal(clicker, toggle, true);
			}

		}, 'html');

		$clicker.on('click');
	};

	if (now === true) {
		setup(url);
	} else {
		$clicker.on('click.ajaxModal', function () {
			setup(url);
		});
	}
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