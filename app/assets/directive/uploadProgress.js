'use strict';

module.directive('uploadProgress', [
	'pageService', function (page) {
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
