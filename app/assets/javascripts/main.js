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

/**
 * FORM HANDLER
 */
(function ($, window, document) {
	var $handler;

	$.extend($.fn, {
		formHandler: function (args) {
			//options
			args = $.extend({
				speed: 150
			}, args);

			$handler = this;


			// methods
			var initialize = function () {
				generate('form');
			};

			var generate = function (element) {
				var $forms = $(element),
					forms = {}, $form;

				$forms.each(function () {
					if ($form = $(this).formHelper(args))
						forms[$form.attr('id')] = $form;
				});

				return forms;
			};

			// hooks
			initialize();

			// api
			this.data('formHandler', {
				generate: generate
			});

			return this;
		},

		formHelper: function (args) {
			//options
			var options,
				defaults = {
					speed: 150
				};

			var $this = this,
				$messages = {},
				$repeaters = {};

			// methods
			var update = function (args) {
				options = $.extend(defaults, args);

				$messages = getMessages();
				$repeaters = getRepeaters();

				setClickers();
				setAjax();

				return $this;
			};

			var getMessages = function () {
				var $messages = $this.find('.message'),
					messages = {};

				$messages.each(function () {
					var $message = $(this),
						$input = $($message.parent().attr('data-for'));

					if ($messageHandler && ($message = $messageHandler.data('messageHandler').create($input, $message)))
						messages[$message.attr('id')] = $message;
				});

				return messages;
			};

			var getRepeaters = function () {
				var $repeaters = $this.find('.repeater'),
					repeaters = {};

				$repeaters.each(function () {
					var $repeater = $(this);

					if ($repeater = $repeater.formRepeater({}))
						repeaters[$repeater.attr('id')] = $repeater;
				});

				return repeaters;
			};

			var setClickers = function () {
				var check = function ($clicker) {
					var $input = $clicker.find('input');

					if ($input.prop('checked'))
						$clicker.addClass('check');
					else
						$clicker.removeClass('check');
				};

				$this.on('click', '.clicker', function (e) {
					var $clicker = $(this),
						$input = $clicker.find('input');

					$input.trigger('click');
					check($clicker);

					e.stopPropagation();
				});

				$this.find('.clicker').each(function () {
					check($(this));
				});
			};

			var $last = null;

			var setAjax = function () {
				if (!$this.hasClass('ajax'))
					return;

				$this.on('focus', ':input', function () {
					$last = $(this);
				});

				$this.submit(function (e) {
					var $this = $(this),
						$li = $this.closest('li'),
						liID = '#' + $li.attr('id');

					$.post($this.attr('action'), {
						name: $this.find('[name="name"]').val(),
						vote: $last.val()
					}, function (data) {
						var $data = $(data);

						$li.replaceWith($data.find(liID));

						$formHandler.data('formHandler').generate(liID + ' form');
					});

					e.preventDefault();
				});
			};

			// setup
			if (!update(args))
				return false;

			// api
			$this.data('formHelper', {
				update: update,
				messages: $messages
			});

			return $this;
		},

		formRepeater: function (args) {
			var $repeater = this,
				$repeats, repeatCount, $copy, newIndex = 0;

			var $tempControls = $('<div class="controls"></div>'),
				$tempControlsCreate = $('<div class="mod create">+</div>'),
				$tempControlsRemove = $('<div class="mod remove">-</div>'),
				$tempControlsMove = $('<div class="move"></div>');

			// methods
			var initialize = function () {
				updateRepeater($repeater);

				$repeats.each(function (index) {
					var $repeat = $(this),
						key = $repeat.find('label[for]').attr('for').split('__').shift().split('_').pop();

					if (key == repeatCount - 1)
						$copy = $repeat.clone();

					updateControls($repeat, index);

					newIndex++;
				});
			};

			var updateRepeater = function ($repeater) {
				$repeats = $repeater.find('.repeat');
				repeatCount = $repeats.length;
			};

			var updateControls = function ($repeat, index) {
				$repeat.find('.controls').remove();

				var $controls = $tempControls.clone();

				$controls.append($tempControlsRemove.clone());

				if (repeatCount >= 2)
					$controls.append($tempControlsMove.clone());

				if (index == repeatCount - 1)
					$controls.append($tempControlsCreate.clone());

				$repeat.append($controls);
			};

			var create = function ($repeater) {
				var cID = $copy.find('label[for]').attr('for').split('__').shift().split('_').pop();

				$repeater.append($($('<div>').append($copy).html().replace(new RegExp('_' + cID + '_', 'g'), '_' + newIndex + '_').replace(new RegExp('\\[' + cID + '\\]', 'g'), '[' + newIndex + ']')));

				updateRepeater($repeater);

				$repeats.each(function (index) {
					updateControls($(this), index);
				});

				newIndex++;
			};

			var remove = function ($repeater, $repeat) {
				if (repeatCount > 1)
					$repeat.remove();
				else
					$repeat.find('input, textarea').val('');

				updateRepeater($repeater);

				$repeats.each(function (index) {
					updateControls($(this), index);
				})
			};

			// setup
			initialize();

			$repeater.on('click', '.controls .remove', function () {
				remove($repeater, $(this).closest('.repeat'));
			});

			$repeater.on('click', '.controls .create', function () {
				create($repeater);
			});

			return $repeater;
		}
	});
})(jQuery, window, document);