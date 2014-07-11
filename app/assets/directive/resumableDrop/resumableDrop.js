"use strict";
module.directive('resumableDrop', [
	'pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.resumable.makeResumable({
				target: $attrs.uploadTarget, 
				method: 'octet', 
				maxFiles: 1, 
				testChunks: false
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

