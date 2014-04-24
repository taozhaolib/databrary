module.factory('Asset', [
	'resourceService', '$route', function (resource, $route) {
		return resource('asset', '/api/asset/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}
]);
