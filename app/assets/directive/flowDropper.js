"use strict";
module.directive('flowDropper', ['pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.flow.makeFlow(); 
			var prepCall = page.flow.makePrepCall();
			var uploadCall = page.flow.makeUploadCall(r);
			var assetCall = page.flow.makeAssetCall($attrs.assetTarget, $attrs.volume);
			var lastToken;
			r.assignDrop($el);
			
			r.on('fileAdded', function(file){
				var x = prepCall(file);
				x.then(function(){
					lastToken = file.uniqueIdentifier;
					uploadCall();
				});
			});

			r.on('complete', function(){
				var data = {};
				data.name = $scope.asset.name;
				data.classification = page.constants.data.classification.indexOf($scope.asset.classification);  //TODO: improve this!
				data.container = $scope.volumeEditMaterialsForm.slot.container.id;
				data.upload = lastToken;
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

