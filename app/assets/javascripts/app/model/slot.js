define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Slot', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/slot/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		});
	}]);
});
