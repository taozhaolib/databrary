define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/volume/:id', {
			id: '@id'
		});
	}]);
});
