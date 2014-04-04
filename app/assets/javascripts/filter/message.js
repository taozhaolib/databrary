define(['config/module'], function (module) {
	'use strict';

	module.filter('message', ['pageService', function (page) {
		return page.constants.message;
	}]);
});
