define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('modeClient', function () {
		var compile = function ($element) {
			$element.replaceWith($element.html());
		};

		return {
			restrict: 'A',
			compile: compile
		};
	});

	db.directive('modeServer', function () {
		var compile = function ($element) {
			$element.remove();
		};

		return {
			restrict: 'A',
			compile: compile
		};
	});
});
