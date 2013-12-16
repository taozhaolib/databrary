define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', function ($scope, authService) {
		$scope.authService = authService;

		$scope.updateComments = function () {
			$scope.comments = $scope.volume.comments;
		};

		$scope.$watch('volume', function () {$scope.updateComments();}, true);
	}]);
});
