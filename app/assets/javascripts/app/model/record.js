define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Record', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/record/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}]);
});
