define(['config/module'], function (module) {
	'use strict';

	module.controller('ResetView', ['$scope', 'PageService', 'ConstantService', function ($scope, page, constants) {
		page.title = constants.message('page.title.reset');
	}]);
});
