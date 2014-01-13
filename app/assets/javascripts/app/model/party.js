define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Party', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/party/:id', {
			id: '@id'
		});
	}]);
});
