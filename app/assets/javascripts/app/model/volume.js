define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/volume/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}]);
});
