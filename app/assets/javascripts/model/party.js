define(['config/module'], function (module) {
	'use strict';

	module.factory('Party', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/party/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, {
			password: {
				method: 'POST',
				url: '/api/party/:id/password'
			}
		});
	}]);
});
