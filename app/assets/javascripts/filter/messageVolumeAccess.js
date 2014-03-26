define(['config/module'], function (module) {
	'use strict';

	module.filter('messageVolumeAccess', ['ConstantService', function (constantService) {
		return constantService.message;
	}]);
});
