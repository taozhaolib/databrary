define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Asset', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/asset/:id', {
			id: '@id'
		});
	}]);
});
