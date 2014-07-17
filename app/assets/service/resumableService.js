"use strict";
/*globals Resumable*/
module.factory('resumableService', [
	'$rootScope', '$http',

	//closure fucntions returning each of three calls

	function ($rootScope, $http){
		var resumableS = {};
		var lastToken;

		resumableS.makeResumable = function(t) {
			return new Resumable({
				target: t,
				method: 'octet', 
				maxFiles: 1, 
				testChunks: false,
				chunkRetryInterval: 5000,
				permanentErrors: [400,403,404,415,500,501]});
		};

		resumableS.makePrepCall = function(target, volume){
			return function(file){
				var x = $http.post(target+
						'?volume='+volume+
						';filename='+file.fileName+
						';size='+file.size);
				
				x.then(function(res){
						file.uniqueIdentifier = res.data;
						lastToken = res.data;
				});
				return x;
			};
		};

		resumableS.makeUploadCall = function(r) {return function(){r.upload();};};

		resumableS.makeAssetCall = function(target, volume){
			return function(data){
				data.upload = lastToken;
				$http.post(target+'?volume='+volume, data);
			};
		};


		resumableS.isSupported = function() {return (new Resumable()).support;};

		return resumableS;
	}
]);

