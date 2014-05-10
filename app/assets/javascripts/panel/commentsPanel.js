module.controller('CommentsPanel', [
	'$scope', 'pageService', function ($scope, page) {
		var DEFAULT_MESSAGE = {
			type: 'blue',
			countdown: 3000
		};

		//

		var createMessage = function (message) {
			if (typeof(message) == 'string') {
				page.messages.add(angular.extend({}, DEFAULT_MESSAGE, {
					body: message
				}));
			}
			else {
				page.messages.add(angular.extend({}, DEFAULT_MESSAGE, message));
			}
		};

		//

		$scope.refreshPanel = function () {
			switch (page.$route.current.controller) {
				case 'VolumeView':
					$scope.comments = $scope.volume.comments;

					$scope.enabled = page.auth.isLoggedIn() || !$.isEmptyObject($scope.comments);
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
					page.models.Volume.$cache.removeAll();

					page.models.Volume.get({
						id: $scope.volume.id,
						comments: ''
					}, function (data) {
						$scope.volume.comments = data.comments;
						$scope.refreshPanel();
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('comments.update.error'),
							errors: res[0],
							status: res[1]
						})
					});

					break;
			}
		};

		//

		$scope.authService = page.auth;
		$scope.routeController = page.$route.current.controller;

		//

		$scope.commentParty = function (comment) {
			switch (page.$route.current.controller) {
				case 'PartyView':
					return $scope.party;

				case 'VolumeView':
				case 'SlotView':
				default:
					return comment.who;
			}
		};

		$scope.commentMeta = function (comment) {
			var isParty = page.$route.current.controller == 'PartyView' && !$scope.volume;
			var isTop = comment.container.top;

			var meta = '<time datetime="' + $filter('date')(comment.time, 'yyyy-MM-dd HH:mm:ss Z') + '" pubdate>' + page.$filter('date')(comment.time, 'MMMM d, yyyy') + '</time>';

			if (isTop && !isParty) {
				return meta;
			}

			meta += ' <span class="sep">|</span>';

			var volumeID = isParty ?
				(comment.volume ? comment.volume.id : 0) :
				($scope.volume ? $scope.volume.id : 0);

			if (isParty) {
				meta += ' <a href="' + page.router.volume({id: volumeID}) + '">' + $filter('truncate')(comment.volume.name || $scope.volume.name, 20) + '</a>';
			}

			if (isParty && !isTop) {
				meta += ' <span class="sep">/</span>';
			}

			if (!isTop) {
				meta += ' <a href="' + page.router.volume({id: volumeID}) + '"><img class="line" src="' + page.router.slotThumb(comment.container) + '"> ' + (comment.container.name || '') + '</a>';
			}

			return meta;
		};

		//

		var commentReplyForm = undefined;
		var replyTo = undefined;

		$scope.getReply = function (comment) {
			return page.auth.isLoggedIn() &&
				page.$route.current.controller != 'PartyView' &&
				replyTo == comment;
		};

		$scope.setReply = function (comment) {
			replyTo = comment;
		};

		//

		var successFn = function () {
			$scope.pullComments();
		};

		var cancelFn = function () {
			$scope.setReply(undefined);
		};

		page.events.listen($scope, 'commentReplyForm-init', function (event, form) {
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
				if (!comment.parent) {
					comment.parent = 0;
				}

				var index = parents.indexOf(comment.parent);

				if (index > -1) {
					parents = parents.slice(0, index + 1);
				}
				else {
					parents.push(comment.parent);
				}

				if (parents.length >= 5) {
					comment.stop = true;
				}

				classes.push('depth-' + (parents.length - 1));
			} else {
				comment.stop = true;
			}

			return classes;
		}
	}
]);
