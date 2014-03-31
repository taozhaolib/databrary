define(['config/module'], function (module) {
	'use strict';

	module.factory('TooltipService', ['$rootScope', 'ArrayHelper', '$timeout', function ($rootScope, arrayHelper, $timeout) {
		var tooltips = arrayHelper([]);
		var $doc = $(document);

		tooltips.types = ['blue', 'green', 'red', 'orange', 'yellow', 'purple'];

		var pad = 10;

		//

		tooltips.newCatalog('id');

		tooltips.newTransform(function (tooltip) {
			tooltip.id = tooltip.id || 'tooltip_' + Math.random().toString(36).substring(2);
			tooltip.cls = tooltip.cls || '';
			tooltip.style = angular.isObject(tooltip.style) ? tooltip.style : {};
			tooltip.type = tooltips.types.indexOf(tooltip.type) != -1 ? tooltip.type : 'blue';
			tooltip.enabled = angular.isUndefined(tooltip.enabled) || tooltip.enabled != false;
			tooltip.visible = !!tooltip.visible || false;
			tooltip.$target = tooltip.$target ? tooltip.$target : undefined;
			tooltip.live = tooltip.live || false;

			tooltip.message = tooltip.message || undefined;

			return tooltip;
		});

		tooltips.newValidate(function (tooltip) {
			return angular.isObject(tooltip) &&
				tooltip.id && tooltip.type && tooltip.$target &&
				angular.isString(tooltip.message) &&
				tooltip.message.length > 0 ? tooltip : false;
		});

		//

		var addFn = tooltips.add;

		tooltips.add = function (tooltip) {
			var newTooltip = addFn(tooltip);

			if (newTooltip)
				tooltips.target(newTooltip);

			return newTooltip;
		};

		//

		var removeFn = tooltips.remove;

		tooltips.remove = function (tooltip) {
			removeEvents(tooltip);

			return removeFn(tooltip);
		};

		//

		var updateFn = tooltips.update;

		tooltips.update = function (tooltip, obj) {
			var newtooltip = updateFn(tooltip, obj);

			if (newtooltip)
				tooltips.target(newtooltip);

			return newtooltip;
		};

		//

		tooltips.enable = function (tooltip) {
			return tooltips.toggle(tooltip, 'enabled', true);
		};

		tooltips.disable = function (tooltip) {
			tooltips.hide(tooltip);

			return tooltips.toggle(tooltip, 'enabled', false);
		};

		//

		tooltips.show = function (tooltip, event) {
			if (!tooltip.enabled)
				return undefined;

			tooltips.position(tooltip, [event.clientX, event.clientY]);

			return tooltips.toggle(tooltip, 'visible', true);
		};

		tooltips.hide = function (tooltip, event) {
			tooltips.position(tooltip, false);

			return tooltips.toggle(tooltip, 'visible', false);
		};

		tooltips.position = function (tooltip, loc) {
			if (loc === false)
				return;

			var $t = tooltip.$target,
				$e = $('#' + tooltip.id),
				$w = $(window);

			if (!loc)
				loc = [
					$t.offset().left,
					$t.offset().top
				];

			var center = {
				left: loc[0],
				top: loc[1],
				right: $w.width() - loc[0],
				bottom: $w.height() - loc[1]
			};

			tooltip.position = [];

			if (center.left > center.right) {
				tooltip.style.left = (loc[0] + $(window).scrollLeft() - $e.outerWidth() - pad) + 'px';
				tooltip.position.push('left');
			} else {
				tooltip.style.left = (loc[0] + $(window).scrollLeft() + pad) + 'px';
				tooltip.position.push('right');
			}

			if (center.top > center.bottom) {
				tooltip.style.top = (loc[1] + $(window).scrollTop() - $e.outerHeight() - pad) + 'px';
				tooltip.position.push('top');
			} else {
				tooltip.style.top = (loc[1] + $(window).scrollTop() + pad) + 'px';
				tooltip.position.push('bottom');
			}
		};

		//

		var getTargetEvents = function (tooltip) {
			if (!tooltip.$target)
				return [];

			var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'],
				namespace = '.tooltipTarget';

			if (!angular.isString(tooltip.$target) && focusElements.indexOf(tooltip.$target.prop('tagName')) >= 0)
				return [
						'focusin' + namespace + '_' + tooltip.id,
						'focusout' + namespace + '_' + tooltip.id
				];

			return [
					'mouseenter' + namespace + '_' + tooltip.id,
					'mouseleave' + namespace + '_' + tooltip.id
			];
		};

		var removeEvents = function (tooltip) {
			if (tooltip.$target)
				if (tooltip.live)
					$doc.off(getTargetEvents(tooltip).join(' '), tooltip.$target);
				else
					tooltip.$target.unbind(getTargetEvents(tooltip).join(' '));
		};

		tooltips.target = function (tooltip, $newTarget) {
			if (tooltips.index(tooltip) == -1)
				return undefined;

			removeEvents(tooltip);

			tooltip.$target = angular.isDefined($newTarget) ? $newTarget : tooltip.$target;

			var $target = tooltip.$target;

			if (!tooltip.live && $target.length === 0) {
				tooltips.disable(tooltip);
				return tooltip.$target = false;
			}

			var events = getTargetEvents(tooltip);

			if (tooltip.live) {
				$doc.on(events[0], tooltip.$target, function (event) {
					$rootScope.$apply(function () {
						tooltips.show(tooltip, event);
					});
				});

				$doc.on(events[1], tooltip.$target, function (event) {
					$rootScope.$apply(function () {
						tooltips.hide(tooltip, event);
					});
				});
			} else {
				$target.bind(events[0], function (event) {
					$rootScope.$apply(function () {
						tooltips.show(tooltip, event);
					});
				});

				$target.bind(events[1], function (event) {
					$rootScope.$apply(function () {
						tooltips.hide(tooltip, event);
					});
				});
			}

			tooltips.hide(tooltip);

			return tooltip;
		};

		//

		$rootScope.$watch(function () {
			angular.forEach(tooltips, function (tooltip) {
				if (!angular.isString(tooltip.$target) && tooltip.$target.closest(document.documentElement).length == 0) {
					removeEvents(tooltip);
					tooltips.remove(tooltip);
				}
			});
		});

		//

		return tooltips;
	}]);
});
