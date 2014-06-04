module.factory('Asset', [
	'$resource', '$route', function ($resource, $route) {
		return $resource('/api/asset/:id', {}, {
			upload: {
				method: 'POST',
				transformRequest: angular.identity,
				headers: {
					'Content-Type': undefined
				},
			}
		});
	}
]);
