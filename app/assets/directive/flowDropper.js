"use strict";
module.directive('flowDropper', ['pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.flow.makeFlow($attrs.uploadTarget); //or do some singleton-y stuff in service?
			var prepCall = page.flow.makePrepCall($attrs.prepTarget, $attrs.volume);
			var uploadCall = page.flow.makeUploadCall(r);
			var assetCall = page.flow.makeAssetCall($attrs.assetTarget, $attrs.volume);
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
						
			$scope.flowObj = r;
		};

		return {
			restrict: 'A',
			scope: true,
			templateUrl: 'flowDropper.html',
			replace: false,
			link: link
		};
	}
]);

