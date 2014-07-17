"use strict";
module.directive('resumableDrop', ['pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.resumable.makeResumable($attrs.uploadTarget); //or do some singleton-y stuff in service?
			var prepCall = page.resumable.makePrepCall($attrs.prepTarget, $attrs.volume);
			var uploadCall = page.resumable.makeUploadCall(r);
			var assetCall = page.resumable.makeAssetCall($attrs.assetTarget, $attrs.volume);
			r.assignDrop($el);
			
			r.on('fileAdded', function(file){
				var x = prepCall(file);
				x.then(uploadCall);
			});

			r.on('complete', function(){
				var data = {};
				data.name = $scope.asset.name;
				data.classification = page.constants.data.classification.indexOf($scope.asset.classification);  //TODO: improve this!
				data.container = $scope.volumeEditMaterialsForm.slot.container.id;
				assetCall(data);
			});
						
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

