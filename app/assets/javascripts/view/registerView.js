define(['config/module'], function (module) {
	'use strict';

	module.controller('RegisterView', ['$scope', 'pageService', function ($scope, page) {
		page.title = page.constants.message('page.title.register');
	}]);
});
