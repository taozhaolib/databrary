module.factory('Volume', [
	'resourceService', '$route', function (resource, $route) {
		return resource('volume', '/api/volume/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}
]);
