define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Comment', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/comment/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		}, {
			'query': {method: 'GET', isArray: false}
		});
	}]);
});
