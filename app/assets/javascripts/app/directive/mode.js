define(['app/config/module'], function (module) {
	'use strict';

	module.directive('modeClient', function () {
		var compile = function ($element) {
			$element.replaceWith($element.html());
		};

		return {
			restrict: 'A',
			compile: compile
		};
	});

	module.directive('modeServer', function () {
		var compile = function ($element) {
			$element.remove();
		};

		return {
			restrict: 'A',
			compile: compile
		};
	});
});
