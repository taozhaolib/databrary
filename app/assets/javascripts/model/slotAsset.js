module.factory('SlotAsset', [
	'resourceService', '$route', function (resource, $route) {
		return resource('slotAsset', '/api/slot/:slotId/asset/:id', {
			slotId: function () {
				return $route.current.params.slotId || undefined;
			},
			id: function () {
				return $route.current.params.id || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		});
	}
]);
