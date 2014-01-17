define(['app/config/module'], function (module) {
	'use strict';

	module.controller('SearchView', ['$scope', 'volumes', 'BrowserService', 'Volume', function ($scope, volumes, browserService, Volume) {
		$scope.browser = browserService;

		$scope.browser.setData(volumes);
		$scope.browser.initialize('search');
	}]);
});
