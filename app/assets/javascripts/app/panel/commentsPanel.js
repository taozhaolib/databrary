define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Volume', '$routeParams', 'Party', 'Slot', function ($scope, authService, $route, Volume, $routeParams, Party, Slot) {
		$scope.routeController = $route.current.controller;

		$scope.bootPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.volume = Volume.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});

					$scope.$watch('volume', function () {
						$scope.automatePanel();
					}, true);
					break;

				case 'SlotView':
					$scope.slot = Slot.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});

					$scope.$watch('slot', function () {
						$scope.automatePanel();
					}, true);
					break;

				case 'PartyView':
					$scope.party = Party.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});

					$scope.$watch('party', function () {
						$scope.automatePanel();
					}, true);
					break;
			}
		};

		//

		$scope.automatePanel = function () {
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
