module.factory('Volume', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('/api/volume/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, {
			create: {
				method: 'POST',
				url: '/api/volume/:owner'
			}
		}, 'volume');
	}
]);
