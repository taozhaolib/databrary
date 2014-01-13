define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Slot', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/slot/:id', {
			id: '@id'
		});
	}]);
});
