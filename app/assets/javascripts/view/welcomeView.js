define(['config/module'], function (module) {
	'use strict';

	module.controller('WelcomeView', ['$scope', 'pageService', function ($scope, page) {
		page.title = page.constants.message('page.title.welcome');
	}]);
});
