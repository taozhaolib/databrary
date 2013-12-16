define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$rootScope', '$resource', '$filter', function ($rootScope, $resource, $filter) {
		return $resource('/api/volume/:id', {
			id: '@id'
		});
	}]);
});
