define(['config/module'], function (module) {
	'use strict';

	module.controller('WelcomeView', ['$scope', 'PageService', 'ConstantService', function ($scope, page, constants) {
		page.title = constants.message('page.title.welcome');

	}]);
});
