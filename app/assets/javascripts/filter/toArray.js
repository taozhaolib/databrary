define(['config/module'], function (module) {
	'use strict';

	module.filter('toArray', ['$filter', function ($filter) {
		return function (input, expression, reverse) {
			if(!angular.isObject(input))
				return [input];

			var output = [];

			angular.forEach(input, function (item) {
				output.push(item);
			});

			if(angular.isDefined(expression))
				output = $filter('orderBy')(output, expression, reverse);

			return output;
		};
	}]);
});
