define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Volume', '$routeParams', 'Party', 'Slot', function ($scope, authService, $route, Volume, $routeParams, Party, Slot) {
		$scope.refreshPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.comments = $scope.volume.comments;
					$scope.enabled = !!authService.user || (angular.isArray($scope.comments) && $scope.comments.length > 0);
					break;

				case 'SlotView':
					$scope.comments = $scope.slot.comments;
					$scope.enabled = !!authService.user || (angular.isArray($scope.comments) && $scope.comments.length > 0);
					break;

				case 'PartyView':
					$scope.comments = $scope.party.comments;
					$scope.enabled = angular.isArray($scope.comments) && $scope.comments.length > 0;
					break;
			}
		};

		//

		$scope.authService = authService;
		$scope.routeController = $route.current.controller;

		//

		$scope.commentParty = function (comment) {
			switch ($route.current.controller) {
				case 'PartyView':
					return $scope.party;

				case 'VolumeView':
				case 'SlotView':
				default:
					return comment.who;
			}
		};
	}]);
});
