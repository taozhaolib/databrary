define(['app/config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Comment', 'MessageService', function ($scope, authService, $route, Comment, messageService) {
		var DEFAULT_MESSAGE = {
			type: 'alert',
			countdown: 3000
		};

		//

		var createMessage = function (message) {
			if (typeof(message) == 'string')
				messageService.createMessage(angular.extend({}, DEFAULT_MESSAGE, {
					message: message
				}));
			else
				messageService.createMessage(angular.extend({}, DEFAULT_MESSAGE, message));
		};

		//

		$scope.target = {
			container: null,
			segment: ','
		};

		$scope.refreshPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.comments = $scope.volume.comments;
					$scope.target.container = $scope.volume.top.id;
					$scope.target.segment = ',';
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

		//

		$scope.newComment = {
			body: ''
		};

		$scope.addComment = function (form) {
			if (form.$invalid)
				return;

			var commentModel = new Comment({});

			commentModel.$save({
				text: $scope.newComment.body,
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (newComment, status, headers, config) {
				createMessage('Comment added successfully!');
				$scope.newComment.body = '';
			});
		};
	}]);
});
