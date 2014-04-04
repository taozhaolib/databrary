define(['config/module'], function (module) {
	'use strict';
	module.filter('format', ['$filter', function ($filter) {
		return function (input, lineMode, raw) {
			if (!angular.isString(input))
				return '';

			if (!raw)
				input = $filter('escape')(input);

			if (lineMode)
				return input.replace(/: /g, ': <br>');

			return '<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
		};
	}]);
});
