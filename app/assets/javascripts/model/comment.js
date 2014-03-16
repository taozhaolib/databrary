define(['config/module'], function (module) {
	'use strict';

	module.factory('Comment', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/comment/:id', {
			segment: function () {
				return $route.current.params.segment || ',';
			},
			container: function () {
				return $route.current.params.container || ',';
			}
		});
	}]);
});
