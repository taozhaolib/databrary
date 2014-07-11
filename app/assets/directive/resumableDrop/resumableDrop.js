"use strict";
module.directive('resumableDrop', [
	'pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.resumable.makeResumable({
				target: $attrs.uploadTarget, 
				method: 'octet', 
				maxFiles: 1, 
				testChunks: false,
				chunkRetryInterval: 5000,
				permanentErrors: [400,403,404,415,500,501]
			});
			
			r.assignDrop($el);
			
			r.on('fileAdded', function(file){
						page.$http.post($attrs.prepTarget+
							'?volume='+$scope.volumeEditMaterialsForm.volume.id+
							';filename='+file.fileName+
							';size='+file.size).then(function(res){
									file.uniqueIdentifier = res.data;
								}
							);
					}
			);

			r.on('complete', function(){
				//call to api/asset with remaining fields
				}
			);
						
			$scope.resumableObj = r;
		};

		return {
			restrict: 'A',
			scope: true,
			templateUrl: 'resumableDrop.html',
			replace: false,
			link: link
		};
	}
]);

