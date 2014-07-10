'use strict';

module.factory('record', [
	'$resource', '$route', function ($resource, $route) {
		return $resource('/api/record/:id', {
			id: function () {
				return $route.current.params.id || undefined;
			}
		});
	}
]);
