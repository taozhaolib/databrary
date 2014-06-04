module.factory('Asset', [
	'$resource', '$http', function ($resource, $http) {
		var asset = $resource('/api/asset/:id');

		asset.upload = function (volume, fd) {
			return $http.post('/api/asset?volume=' + volume.id, fd, {
				transformRequest: angular.identity,
				headers: {
					'Content-Type': undefined
				},
			});
		};

		return asset;
	}
]);
