define(['config/module'], function (module) {
	'use strict';

	module.controller('CommentsPanel', ['$scope', 'AuthService', '$route', 'Comment', 'MessageService', 'Volume', '$filter', '$cacheFactory', 'EventService', function ($scope, authService, $route, Comment, messageService, Volume, $filter, $cacheFactory, events) {
		var DEFAULT_MESSAGE = {
			type: 'blue',
			countdown: 3000
		};

		var $httpCache = $cacheFactory.get('$http');

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

		$scope.refreshPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.comments = $scope.volume.comments;

					$scope.enabled = authService.isLoggedIn() || !$.isEmptyObject($scope.comments);
					break;

				case 'PartyView':
					$scope.comments = $scope.party.comments;

					$scope.enabled = !$.isEmptyObject($scope.comments);
					break;
			}
		};

		//

		$scope.pullComments = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$httpCache.removeAll();

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

			var volumeID = $route.current.controller == 'PartyView' ? (comment.volume ? comment.volume.id : 0) : ($scope.volume ? $scope.volume.id : 0);

			if ($route.current.controller == 'PartyView')
				meta += ' <a href="' + $scope.router.volume({id: volumeID}) + '">' + $filter('truncate')(comment.volume.name, 20) + '</a>';

			if ($route.current.controller == 'PartyView' && !comment.container.top)
				meta += ' <span class="sep">/</span>';

			if (!comment.container.top)
				meta += ' <a href="' + $scope.router.volume({id: volumeID}) + '"><img class="line" src="' + $scope.router.slotThumb(comment.container) + '"> ' + (comment.container.name || '') + '</a>';

			return meta;
		};

		//

		var commentReplyForm = undefined;
		var replyTo = undefined;

		$scope.getReply = function (comment) {
			return authService.isLoggedIn() &&
				$route.current.controller != 'PartyView' &&
				replyTo == comment;
		};

		$scope.setReply = function (comment, volume) {
			replyTo = comment;
		};

		//

		var successFn = function () {
			createMessage('Comment added successfully!');
			$scope.pullComments();
		};

		var errorFn = function () {
			createMessage({
				body: 'This comment is unacceptable!',
				type: 'red'
			});
		};

		var cancelFn = function () {
			$scope.setReply(undefined);
		};

		events.listen($scope, 'commentReplyForm-init', function (event, form) {
			commentReplyForm = form;
			form.successFn = successFn;
			form.cancelFn = cancelFn;
			form.target(replyTo);
			event.stopPropagation();
		});

		//

		var parents = [0];

		$scope.getCommentClasses = function (comment) {
			var classes = [];

			if ($route.current.controller != 'PartyView') {
				if (!comment.parent)
					comment.parent = 0;

				var index = parents.indexOf(comment.parent);

				if (index > -1)
					parents = parents.slice(0, index + 1);
				else
					parents.push(comment.parent);

				if (parents.length >= 5)
					comment.stop = true;

				classes.push('depth-' + (parents.length - 1));
			}else{
				comment.stop = true;
			}

			return classes;
		}
	}]);
});
