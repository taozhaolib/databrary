define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', function ($scope, authService, $route) {
		$scope.authService = authService;

		//

		$scope.commentParty = function (comment) {
			switch($scope.view.view) {
				case 'volume':
					return comment.who;
					break;
				case 'party':
					return $scope.party;
					break;
			}
		};

		$scope.commentVolume = function (comment) {
			switch($scope.view.view) {
				case 'volume':
					return $scope.volume;
					break;
				case 'party':
					return {
						id: 0,
						name: 'NULL'
					};
					break;
			}
		};

		//

		$scope.updateComments = function () {
			switch($scope.view.view) {
				case 'volume':
					$scope.comments = $scope.volume.comments;
					break;
				case 'party':
					$scope.comments = $scope.party.comments;
					break;
			}
		};

		$scope.$watch('view', function () {$scope.updateComments();}, true);
	}]);
});
