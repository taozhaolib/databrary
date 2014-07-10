'use strict';

module.factory('Comment', [
	'$resource', '$route', function ($resource, $route) {
		return $resource('/api/comment/:id', {
			segment: function () {
				return $route.current.params.segment || ',';
			},
			container: function () {
				return $route.current.params.container || ',';
			}
		});
	}
]);
