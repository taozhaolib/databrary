'use strict';

module.directive('uploadProgress', [
	function () {
		var link = function($scope) {
			$scope.progressFloat = undefined;
		};

		return {
			restrict: 'E',
			scope: {
				progressFloat: '=progressValue'
			},
			templateUrl: 'uploadProgress.html',
			link: link
		};
	}
]);

