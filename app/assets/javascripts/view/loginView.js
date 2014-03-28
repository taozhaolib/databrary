define(['config/module'], function (module) {
	'use strict';

	module.controller('LoginView', ['$scope', 'PageService', 'ConstantService', function ($scope, page, constants) {
		page.title = constants.message('page.title.login');
	}]);
});
