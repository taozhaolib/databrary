define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Asset', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/asset/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}]);
});
