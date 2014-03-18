define(['config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Comment', 'MessageService', 'Volume', '$filter', function ($scope, authService, $route, Comment, messageService, Volume, $filter) {
		var DEFAULT_MESSAGE = {
			type: 'alert',
			countdown: 3000
		};

		//

		var createMessage = function (message) {
			if (typeof(message) == 'string')
				messageService.add(angular.extend({}, DEFAULT_MESSAGE, {
					body: message
				}));
			else
				messageService.add(angular.extend({}, DEFAULT_MESSAGE, message));
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

		$scope.pullComments = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					Volume.get({
						id: $scope.volume.id,
						comments: ''
					}, function (data) {
						$scope.volume.comments = data.comments;
						$scope.refreshPanel();
					});
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

		$scope.commentMeta = function (comment) {
			var meta = '<time datetime="' + $filter('date')(comment.time, 'yyyy-MM-dd HH:mm:ss Z') + '" pubdate>' + $filter('date')(comment.time, 'MMMM d, yyyy') + '</time>';

			if (comment.container.top && $route.current.controller != 'PartyView')
				return meta;

			meta += ' <span class="sep">|</span>';

			if ($route.current.controller == 'PartyView')
				meta += ' <a href="' + $scope.router.volume({id: comment.volume.id}) + '">' + $filter('truncate')(comment.volume.name, 20) + '</a>';

			if ($route.current.controller == 'PartyView' && !comment.container.top)
				meta += ' <span class="sep">/</span>';

			if (!comment.container.top)
				meta += ' <img class="line" src="'+$scope.router.slotThumb(comment.container)+'"> <a href="' + $scope.router.slot(comment.container) + '">' + comment.container.name + '</a>';

			return meta;
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

				$scope.pullComments();
			});
		};
	}]);
});
