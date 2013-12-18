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
						$scope.updateData();
					}, true);
					break;

				case 'SlotView':
					$scope.slot = Slot.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});

					$scope.$watch('slot', function () {
						$scope.updateData();
					}, true);
					break;

				case 'PartyView':
					$scope.party = Party.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});

					$scope.$watch('party', function () {
						$scope.updateData();
					}, true);
					break;
			}
		};

		//

		$scope.updateData = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.comments = $scope.volume.comments;
					break;

				case 'SlotView':
					$scope.comments = $scope.slot.comments;
					break;

				case 'PartyView':
					$scope.comments = $scope.party.comments;
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
					return comment.who;
			}
		};
	}]);
});
