"use strict";
/* globals Flow */
module.factory('flowService', [
	'$rootScope', '$http',

	//closure functions returning each of three calls

	function ($rootScope, $http){
		var flowS = {};
		var lastToken;

		flowS.makeFlow = function(t) {
			return new Flow({
				target: t,
				method: 'octet', 
				maxFiles: 1, 
				testChunks: false,
				chunkRetryInterval: 5000,
				permanentErrors: [400,403,404,415,500,501]});
		};

		flowS.makePrepCall = function(target, volume){
			return function(file){
				var x = $http.post(target+
						'?filename='+file.name+
						';size='+file.size);
				
				x.then(function(res){
						file.uniqueIdentifier = res.data;
						lastToken = res.data;
				});
				return x;
			};
		};

		flowS.makeUploadCall = function(f) {return function(){f.upload();};};

		flowS.makeAssetCall = function(target, volume){
			return function(data){
				data.upload = lastToken;
				$http.post(target+'?volume='+volume, data);
			};
		};


		flowS.isSupported = function() {return (new Flow()).support;};

		return flowS;
	}
]);

