"use strict";
/* globals Flow */
module.factory('uploadService', [
	'$rootScope', '$http', 'constantService',

	//closure functions returning each of three calls

	function ($rootScope, $http, constants){
		var uploadS = {};
		var PREP_TARGET = '/api/asset/start';

		uploadS.flowOptions = {
			target: '/api/asset/chunk',
			method: 'octet', 
			maxFiles: 1, 
			testChunks: false,
			chunkRetryInterval: 5000,
			permanentErrors: [400,403,404,415,500,501],
			progressCallbacksInterval: 300,
			prioritizeFirstAndLastChunk: true
		};

		uploadS.prepCall = function(file){
			var x = $http.post(PREP_TARGET, undefined, {
					params: {
						filename:	file.name,
						size:		file.size,
					}
			});

			x.then(function(res){
					file.uniqueIdentifier = res.data;
			});
			return x;
		};

		uploadS.fileAddedImmediateUpload = function(file){
			uploadS.prepCall(file).then(function(){
				file.flowObj.upload();
			});
		};

		uploadS.assetCall = function(target, volumeId, data){
			$http.post(target, data, {params: {volume: volumeId}});
		};

		uploadS.isSupported = function() {return (new Flow()).support;};

		return uploadS;
	}
]);

