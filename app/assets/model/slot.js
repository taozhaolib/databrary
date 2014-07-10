'use strict';

module.factory('Slot', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('/api/volume/:vid/slot/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			vid: function () {
				return $route.current.params.vid || undefined;
			},
			segment: function () {
				return $route.current.params.segment || ',';
			}
		}, 'slot');
	}
]);
