define(['config/module'], function (module) {
	'use strict';

	module.filter('message', ['ConstantService', function (constantService) {
		return constantService.message;
	}]);
});
