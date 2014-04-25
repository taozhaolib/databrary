module.factory('Slot', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('/api/slot/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		}, 'slot');
	}
]);
