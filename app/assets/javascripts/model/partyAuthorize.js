module.factory('PartyAuthorize', [
	'resourceService', '$route', 'authService', function (resource, $route, auth) {
		return resource('partyAuthorize', '/api/party/:id/authorize/:partyId', {
			id: function () {
				return $route.current.params.id ? $route.current.params.id : auth.user.id || undefined;
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
	}
]);
