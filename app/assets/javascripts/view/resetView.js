define(['config/module'], function (module) {
	'use strict';

	module.controller('ResetView', ['$scope', 'Page', 'ConstantService', function ($scope, page, constants) {
		page.title = constants.message('page.title.reset');
	}]);
});
