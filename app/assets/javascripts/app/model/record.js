define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Record', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/record/:id', {
			id: '@id'
		});
	}]);
});
