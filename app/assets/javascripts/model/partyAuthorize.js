define(['config/module'], function (module) {
	'use strict';

	module.factory('PartyAuthorize', ['$resource', '$route', function ($resource, $route) {
		return $resource('/api/party/:id/authorize/:partyId', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, {
			'query': {
				method: 'GET',
				isArray: false
			},
			'search': {
				method: 'GET',
				url: '/api/party/:id/authorize/search'
			},
			'apply': {
				method: 'POST',
				url: '/api/party/:id/authorize/:partyId/apply'
			}
		});
	}]);
});
