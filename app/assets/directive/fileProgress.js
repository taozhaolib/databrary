'use strict';

module.directive('fileProgress', [
	'pageService', function (page) {
		var link = function($scope, $attrs) {
			$scope.progressFloat = undefined;
		};

		return {
			restrict: 'E',
			templateUrl: 'fileProgress.html',
			link: link
		};
	}
]);

