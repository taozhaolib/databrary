define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PublicationsPanel', ['$scope', function ($scope) {
		$scope.updateData = function () {
			switch($scope.view.view) {
				case 'volume':
					$scope.publications = $scope.volume.citations;
					break;
			}
		};

		$scope.$watch('view', function () {$scope.updateData();}, true);
	}]);
});
