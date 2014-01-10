define(['app/config/module'], function (module) {
	'use strict';

	module.controller('PublicationsPanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.bootPanel = function () {
			$scope.volume = Volume.get($routeParams.id, {
				id: $routeParams.id,
				citations: ''
			});

			$scope.$watch('volume', function () {
				$scope.automatePanel();
			}, true);
		};

		$scope.automatePanel = function () {
			$scope.enabled = angular.isArray($scope.volume.citations) && $scope.volume.citations.length > 0;
		};
	}]);
});
