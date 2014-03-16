define(['config/module'], function (module) {
	'use strict';

	module.controller('PublicationsPanel', ['$scope', 'Volume', '$routeParams', function ($scope, Volume, $routeParams) {
		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.citations) && $scope.volume.citations.length > 0;
		};
	}]);
});
