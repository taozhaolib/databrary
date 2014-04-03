define(['config/module'], function (module) {
	'use strict';

	module.controller('TagsPanel', ['$scope', 'Tag', '$route', 'Page', 'Volume', '$cacheFactory', '$http', '$timeout', function ($scope, Tag, $route, page, Volume, $cacheFactory, $http, $timeout) {
		var DEFAULT_MESSAGE = {
			type: 'blue',
			countdown: 3000
		};

		var $httpCache = $cacheFactory.get('$http');

		//

		var createMessage = function (message) {
			if (typeof(message) == 'string')
				page.messages.add(angular.extend({}, DEFAULT_MESSAGE, {
					body: message
				}));
			else
				page.messages.add(angular.extend({}, DEFAULT_MESSAGE, message));
		};

		//

		$scope.tags = [];
		$scope.target = {
			container: null,
			segment: null
		};

		$scope.refreshPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.prepareTags($scope.volume.tags);
					$scope.target.container = $scope.volume.top.id;
					$scope.target.segment = ',';
					$scope.enabled = true;

					$scope.enabled = $scope.tags.length > 0 || $scope.auth.isLoggedIn();
					break;

				case 'PartyView':
					$scope.prepareTags($scope.party.tags);

					$scope.enabled = $scope.tags.length > 0;
					break;
			}
		};

		$scope.prepareTags = function (tags) {
			var temp = [];

			angular.forEach(tags, function (tag) {
				temp.push(tag);
			});

			$scope.tags = temp;

			$scope.sortTags();
		};

		$scope.sortTags = function () {
			$scope.tags = $scope.tags.sort(function (a, b) {
				return (a.weight > b.weight) ? -1 : (a.weight < b.weight) ? 1 : 0;
			});
		};

		$scope.retrieveTags = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$httpCache.removeAll();

					Volume.get({
						id: $scope.volume.id,
						tags: ''
					}, function (data) {
						$scope.volume.tags = data.tags;
						$scope.refreshPanel();
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('tags.update.error'),
							errors: res[0],
							status: res[1]
						})
					});

					break;
			}
		};

		//

		$scope.vote = function (tag, vote) {
			var tagModel = new Tag({id: tag.id});

			tagModel.$save({
				id: tag.id,
				vote: vote == -1 ? 'false' : vote == 1 ? "true" : "",
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (newTag) {
				if (newTag.weight == 0 && !newTag.vote)
					$scope.tags.splice($scope.tags.indexOf(tag), 1);
				else
					$scope.tags.splice($scope.tags.indexOf(tag), 1, newTag);

				switch (vote) {
					case -1:
						createMessage('Tag <strong>' + tag.id + '</strong> voted down successfully!');
						break;

					case 0:
						createMessage('Tag <strong>' + tag.id + '</strong> vote cancelled successfully!');
						break;

					case 1:
						createMessage('Tag <strong>' + tag.id + '</strong> voted up successfully!');
						break;
				}

				hideTooltips();
			}, function () {
				createMessage({
					type: 'red',
					body: 'Vote for tag <strong>' + tag.id + '</strong> unsuccessful! Please refresh and try again.'
				});
				
				hideTooltips();
			});
		};

		$scope.voteNew = function (form) {
			if (form.$invalid)
				return;

			emptyAuto();

			var tagModel = new Tag({id: form.newNameVal});

			tagModel.$save({
				id: form.newNameVal,
				vote: "true",
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (newTag, status, headers, config) {
				createMessage('Tag <strong>' + form.newNameVal + '</strong> added successfully!');

				form.newNameVal = '';
				emptyAuto();

				$scope.retrieveTags();
				hideTooltips();
			}, function () {
				createMessage({
					type: 'red',
					body: 'Could not add tag <strong>' + tag.id + '</strong>! Please refresh and try again.'
				});

				emptyAuto();
				hideTooltips();
			});
		};

		//

		$scope.newNameChange = function (form) {
			if (form.newName.$dirty && form.newName.$valid)
				updateAuto(form);

			if (form.newName.$pristine || form.newName.$valid)
				return disableNewNameError();

			return enableNewNameError();
		};

		var keypress = function (event, form) {
			if (event.which == 40) {
				// down
				if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect == $scope.autoList.length - 1)
					$scope.autoSelect = 0;
				else
					$scope.autoSelect = $scope.autoSelect + 1;

				return;
			} else if (event.which == 38) {
				// up
				if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect == 0)
					$scope.autoSelect = $scope.autoList.length - 1;
				else
					$scope.autoSelect = $scope.autoSelect - 1;

				return;
			} else if (event.which == 13) {
				// enter
				if (angular.isDefined($scope.autoSelect))
					$scope.fillAuto(form, $scope.autoList[$scope.autoSelect]);

			} else if (event.which == 27) {
				// escape
				emptyAuto();
			}
		};

		$scope.newNameFocus = function (form) {
			emptyAuto();

			$('#newName').on('keydown', function (event) {
				$scope.$apply(function () {
					keypress(event, form);
				});
			});

			$scope.newNameChange(form);
		};

		$scope.newNameBlur = function (form) {
			$('#newName').off('keypress');

			emptyAutoAfter(250);

			return disableNewNameError();
		};

		$scope.autoList = [];
		$scope.autoSelect = undefined;

		var updateAuto = function (form) {
			$http
				.get('/api/tag', {
					params: {
						query: form.newNameVal
					}
				}).success(function (data) {
					emptyAuto();

					if (form.newNameVal)
						$scope.autoList = data;
				}).error(function (errors, status) {
					page.messages.addError({
						body: page.constants.message('tags.auto.error'),
						errors: errors,
						status: status
					});

					emptyAuto();
				});
		};

		var emptyAuto = function () {
			$scope.autoList = [];
			$scope.autoSelect = undefined;
		};

		var emptyAutoAfter = function (after) {
			$timeout(function () {
				emptyAuto();
			}, angular.isNumber(after) ? after : 1);
		};

		$scope.fillAuto = function (form, autoTag) {
			form.newNameVal = autoTag;
			emptyAuto();
			$scope.voteNew(form);
		};

		//

		var enableNewNameError = function () {
			emptyAuto();

			if ($scope.tagNewFormMessage) {
				page.messages.enable($scope.tagNewFormMessage);
			} else {
				var message = {
					enabled: true,
					type: 'red',
					body: '<dl>' +
						'<dt>Tag Name</dt>' +
						'<dd>Must be between 3 and 32 characters.</dd>' +
						'<dd>Only letters, spaces, and dashes (-) allowed.</dd>' +
						'</dl>'
				};

				$scope.tagNewFormMessage = page.messages.add(message);
			}
		};

		var disableNewNameError = function () {
			if ($scope.tagNewFormMessage)
				page.messages.disable($scope.tagNewFormMessage);
		};

		//

		var tips = [];

		var bindTooltips = function () {
			var unsetTips = {
				'.panel_tags_list .vote.available.up': page.constants.message('tags.vote.up'),
				'.panel_tags_list .vote.available.null': page.constants.message('tags.vote.null'),
				'.panel_tags_list .vote.available.down': page.constants.message('tags.vote.down')
			};

			angular.forEach(unsetTips, function (message, target) {
				tips.push(page.tooltips.add({
					live : true,
					$target: target,
					message: message
				}));
			});
		};

		bindTooltips();

		var hideTooltips = function () {
			angular.forEach(tips, function (tip) {
				page.tooltips.hide(tip);
			});
		};

		$scope.$on('$destroy', function () {
			angular.forEach(tips, function (tip) {
				page.tooltips.remove(tip);
			});

			tips = [];
		});
	}]);
});
