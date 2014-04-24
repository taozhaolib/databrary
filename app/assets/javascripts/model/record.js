module.factory('Record', [
	'resourceService', '$route', function (resource, $route) {
		return resource('record', '/api/record/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}
]);
