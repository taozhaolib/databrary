module.factory('Slot', [
	'resourceService', '$route', function (resource, $route) {
		return resource('slot', '/api/slot/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		});
	}
]);
