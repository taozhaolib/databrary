define(['config/module'], function (module) {
	'use strict';

	module.controller('LoginView', ['$scope', 'Page', 'ConstantService', function ($scope, page, constants) {
		page.title = constants.message('page.title.login');
	}]);
});
