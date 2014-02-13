define(['app/config/module'], function (module) {
	'use strict';

	module.controller('SearchView', ['$scope', 'volumes', function ($scope, volumes) {
		$scope.volumes = volumes;

		$scope.browser.initialize('search', volumes);
	}]);
});
