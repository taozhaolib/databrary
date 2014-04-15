module.factory('VolumeAccess', [
	'$resource', '$route', function ($resource, $route) {
		return $resource('/api/volume/:id/access/:partyId', {
			id: function () {
				return $route.current.params.id || undefined;
			},
			partyId: function () {
				return $route.current.params.partyId || undefined;
			}
		});
	}
]);
