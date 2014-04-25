module.factory('Party', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('/api/party/:id', {
			id: function () {
				return ($route.current && $route.current.params.id) || undefined;
			}
		}, {
			password: {
				method: 'POST',
				url: '/api/party/:id/password'
			},
			profile: {
				method: 'GET',
				url: '/api/profile'
			},
			user: {
				method: 'GET',
				url: '/api/user'
			},
			login: {
				method: 'POST',
				url: '/api/user/login'
			},
			logout: {
				method: 'POST',
				url: '/api/user/logout'
			},
			superuserOn: {
				method: 'POST',
				url: '/api/user/superuser/on'
			},
			superuserOff: {
				method: 'POST',
				url: '/api/user/superuser/off'
			}
		}, 'party');
	}
]);
