define(['config/module'], function (module) {
	'use strict';

	module.factory('Volume', ['$resource', '$route', function ($resource, $route) {
		return $resource('/api/volume/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}]);
});