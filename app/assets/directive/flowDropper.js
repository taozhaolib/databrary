"use strict";
module.directive('flowDropper', ['pageService', function (page) {
		var link = function($scope, $el, $attrs) {
			var r = new page.flow.makeFlow(); 
			r.assignDrop($el);
			
			r.on('fileAdded', page.flow.fileAddedImmediateUpload);

			r.on('fileSuccess', page.flow.makeAssetSuccessCall($attrs.assetTarget, $attrs.volume, $scope.volumeEditMaterialsForm.slot.container.id, $scope.asset)); 
						
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

