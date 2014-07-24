"use strict";
/* globals Flow */
module.factory('flowService', [
	'$rootScope', '$http', 'constantService',

	//closure functions returning each of three calls

	function ($rootScope, $http, constants){
		var flowS = {};
		var PREP_TARGET = '/api/asset/start';

		flowS.options = {
			target: '/api/asset/chunk',
			method: 'octet', 
			maxFiles: 1, 
			testChunks: false,
			chunkRetryInterval: 5000,
			permanentErrors: [400,403,404,415,500,501]
		};

		flowS.makeFlow = function() {return new Flow(flowS.options);};

		flowS.prepCall = function(file){
			var x = $http.post(PREP_TARGET+
					'?filename='+file.name+
					';size='+file.size);
		
			x.then(function(res){
					file.uniqueIdentifier = res.data;
			});
			return x;
		};

		flowS.fileAddedImmediateUpload = function(file){
			flowS.prepCall(file).then(function(){
				file.flowObj.upload();
			});
		};

		flowS.makeAssetSuccessCall = function(target, volume, container, asset){
			return function(file){
				var data = {};
				data.name = asset.name;
				data.classification = constants.data.classification.indexOf(asset.classification);  //TODO: improve this!
				data.container = container;
				data.upload = file.uniqueIdentifier;
				$http.post(target+'?volume='+volume, data);
			};
		};

		flowS.assetCall = function(target, volumeId, data){
			$http.post(target, data, {params: {volume: volumeId}});
		};

		flowS.isSupported = function() {return (new Flow()).support;};

		return flowS;
	}
]);

