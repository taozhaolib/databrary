define(['config/module'], function (module) {
	'use strict';

	module.controller('SearchView', ['$scope', 'volumes', 'PageService', 'ConstantService', function ($scope, volumes, page, constants) {
		$scope.volumes = volumes;
		page.title = constants.message('page.title.search');

		$scope.browser.initialize('search', volumes);
	}]);
});
