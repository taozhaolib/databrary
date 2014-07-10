'use strict';

module.directive('hint', [
	'pageService', function (page) {
		var hints = {};

		angular.forEach(page.constants.data.permission, function (a, i) {
			hints['permission-' + a.toLowerCase()] = {
				class: 'hint-permission-' + a.toLowerCase(),
				message: page.constants.message('access.' + a, 'You'),
			};

			if (i >= 3) {
				hints['access-edit-' + a.toLowerCase()] = {
					class: 'hint-access-edit-' + a.toLowerCase(),
					message: page.constants.message('access.edit.' + a + '.who', 'You'),
				};
			}
		});

		angular.forEach(page.constants.data.consent, function (a) {
			hints['consent-' + a.toLowerCase()] = {
				class: 'hint-consent-' + a.toLowerCase(),
				message: page.constants.message('consent.' + a),
			};
		});

		angular.forEach(page.constants.data.classification, function (a) {
			hints['classification-' + a.toLowerCase()] = {
				class: 'hint-classification-' + a.toLowerCase(),
				message: page.constants.message('classification.' + a),
			};
		});

		angular.forEach(page.constants.data.format, function (a) {
			hints['format-' + a.extension.toLowerCase()] = {
				class: 'format-' + a.extension.toLowerCase(),
				message: a.name,
			};
		});

		angular.forEach(['dataset', 'study', 'record', 'session', 'asset'], function (a) {
			hints['object-' + a] = {
				class: 'hint-object-' + a,
				message: page.constants.message('object.tip.' + a),
			};
		});

		angular.forEach(['up', 'null', 'down'], function (a) {
			hints['tags-vote-' + a] = {
				class: 'tags-vote-' + a,
				message: page.constants.message('tags.vote.' + a),
			};
		});

		angular.forEach(hints, function (hint, name) {
			hints[name].tooltip = page.tooltips.add({
				live: true,
				$target: '.' + hint.class,
				message: hint.message
			});
		});

		var link = function ($scope, $element, $attrs) {
			if (angular.isString($attrs.hint) && hints[$attrs.hint.toLowerCase()]) {
				$element.addClass(hints[$attrs.hint.toLowerCase()].class);
			}

			if (angular.isDefined($attrs.hintObserve)) {
				$attrs.$observe('hint', function (val, old) {
					$element.removeClass(hints[old.toLowerCase()].class).addClass(hints[val.toLowerCase()].class);
				});
			}
		};

		return {
			restrict: 'A',
			link: link,
		};
	}
]);
