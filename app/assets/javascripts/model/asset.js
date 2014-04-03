define(['config/module'], function (module) {
	'use strict';

	module.factory('Asset', ['$resource', '$route', function ($resource, $route) {
		return $resource('/api/asset/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}]);
});