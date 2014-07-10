'use strict';

module.factory('volume', [
	'resourceFactory', '$route', function (resource, $route) {
		return resource('/api/volume/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		}, 'volume');
	}
]);
