define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PartyAuthorize', ['$rootScope', '$resource', '$route', function ($rootScope, $resource, $route) {
		return $resource('/api/party/:id/authorize/:partyId', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, {
			'query': {method: 'GET', isArray: false}
		});
	}]);
});
