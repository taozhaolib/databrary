define(['app/modules/dbModule'], function (dbModule) {
	'use strict';

	return dbModule.config(['$routeProvider', function ($routeProvider) {
		$routeProvider.when('/', {});
	}]);
});
