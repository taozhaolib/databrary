'use strict';

module.factory('asset', [
	'$resource', '$http', function ($resource, $http) {
		var asset = $resource('/api/asset/:id');

		asset.oldUpload = function (volume, fd) {
			return $http.post('/api/asset?volume=' + volume.id, fd, {
				transformRequest: angular.identity,
				headers: {
					'Content-Type': undefined
				},
			});
		};

		asset.replace = function (asset, fd) {
			return $http.post('/api/asset/' + asset.asset.id + '/replace', fd, {
				transformRequest: angular.identity,
				headers: {
					'Content-Type': undefined
				},
			});
		};

		asset.fileAddedImmediateUpload = function(file){
			file.pause();
			$http.post('/api/asset/start', undefined, {
					params: {
						filename:	file.name,
						size:		file.size,
					}
			}).then(function(res){
					file.uniqueIdentifier = res.data;
			}).then(function(){
				file.resume();
			});
		};

		asset.assetCall = function(volumeId, data){
			return $http.post('/api/asset', data, {params: {volume: volumeId}});
		};

		asset.flowOptions = {
			target: '/api/asset/chunk',
			method: 'octet', 
			simultaneousUploads: 3, 
			testChunks: false,
			chunkRetryInterval: 5000,
			permanentErrors: [400,403,404,415,500,501],
			progressCallbacksInterval: 300,
			prioritizeFirstAndLastChunk: true 
		};

		return asset;
	}
]);
