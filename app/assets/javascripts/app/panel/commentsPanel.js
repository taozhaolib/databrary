define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Volume', '$routeParams', 'Party', 'Slot', function ($scope, authService, $route, Volume, $routeParams, Party, Slot) {
		$scope.bootPanel = function () {
			switch ($scope.view.view) {
				case 'volume':
					$scope.volume = Volume.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});
					break;

				case 'slot':
					$scope.slot = Slot.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});
					break;

				case 'party':
					$scope.party = Party.get($routeParams.id, {
						id: $routeParams.id,
						comments: 'all'
					});
					break;
			}
		};

		//

		$scope.updateData = function () {
			switch ($scope.view.view) {
				case 'volume':
					$scope.comments = $scope.volume.comments;
					break;

				case 'party':
					$scope.comments = $scope.party.comments;
					break;
			}
		};

		$scope.$watch('view', function () {
			$scope.updateData();
		}, true);

		//

		$scope.authService = authService;

		//

		$scope.commentParty = function (comment) {
			switch ($scope.view.view) {
				case 'volume':
					return comment.who;

				case 'party':
					return $scope.party;
			}
		};

		$scope.commentVolume = function (comment) {
			switch ($scope.view.view) {
				case 'volume':
					return $scope.volume;

				case 'party':
					return {
						id: 0,
						name: 'NULL'
					};
			}
		};
	}]);
});
