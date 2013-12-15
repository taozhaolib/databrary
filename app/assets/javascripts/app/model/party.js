define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Party', ['$rootScope', '$resource', '$filter', function ($rootScope, $resource, $filter) {
		return $resource('/api/party/:id', {
			id: '@id'
		});
	}]);
});
