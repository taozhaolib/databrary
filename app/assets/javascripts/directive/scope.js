define(['config/module'], function (module) {
	'use strict';

	module.directive('scope', [function () {
		return {
			restrict: 'AE',
			scope: true
		};
	}]);
});
