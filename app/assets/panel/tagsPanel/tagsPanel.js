'use strict';

module.controller('TagsPanel', [
	'$scope', 'pageService', function ($scope, page) {
		var DEFAULT_MESSAGE = {
			type: 'green',
			countdown: 3000,
		};

		//

		var createMessage = function (message) {
			if (typeof(message) == 'string') {
				$scope.messages.add(angular.extend({}, DEFAULT_MESSAGE, {
					body: message,
				}));
			}
			else {
				$scope.messages.add(angular.extend({}, DEFAULT_MESSAGE, message));
			}
		};

		//

		$scope.tags = [];
		$scope.target = {
			container: null,
			segment: null
		};

		$scope.refreshPanel = function () {
			switch (page.$route.current.controller) {
				case 'VolumeView':
					$scope.prepareTags($scope.volume.tags);
					$scope.target.container = $scope.volume.top.id;
					$scope.target.segment = ',';
					$scope.enabled = true;

					$scope.enabled = $scope.tags.length > 0 || page.auth.isLoggedIn();
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
			switch (page.$route.current.controller) {
				case 'VolumeView':
					page.models.Volume.$cache.removeAll();

					page.models.Volume.get({
						id: $scope.volume.id,
						tags: ''
					}, function (data) {
						$scope.volume.tags = data.tags;
						$scope.refreshPanel();
					}, function (res) {
						$scope.messages.addError({
							body: page.constants.message('tags.update.error'),
							report: res,
						})
					});

					break;
			}
		};

		//

		$scope.vote = function (tag, vote) {
			var tagModel = new page.models.Tag({id: tag.id});

			tagModel.$save({
				id: tag.id,
				vote: vote == -1 ? 'false' : vote == 1 ? 'true' : '',
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (newTag) {
				if (newTag.weight == 0 && !newTag.vote) {
					$scope.tags.splice($scope.tags.indexOf(tag), 1);
				}
				else {
					$scope.tags.splice($scope.tags.indexOf(tag), 1, newTag);
				}

				switch (vote) {
					case -1:
						createMessage(page.constants.message('tags.vote.down.success', tag.id));
						break;

					case 0:
						createMessage(page.constants.message('tags.vote.null.success', tag.id));
						break;

					case 1:
						createMessage(page.constants.message('tags.vote.up.success', tag.id));
						break;
				}
			}, function (res) {
				$scope.messages.addError({
					body: page.constants.message('tags.vote.error', tag.id),
					report: res,
				});
			});
		};

		$scope.voteNew = function (form) {
			if (form.$invalid || !form.newNameVal) {
				return;
			}

			emptyAuto();

			var tagModel = new page.models.Tag({id: form.newNameVal});
			var data = {
				id: form.newNameVal,
				vote: 'true',
				container: $scope.target.container,
				segment: $scope.target.segment
			};

			form.newNameVal = '';

			tagModel.$save(data, function () {
				createMessage(page.constants.message('tags.new.success', data.id));
				emptyAuto();

				$scope.retrieveTags();
			}, function (res) {
				$scope.messages.addError({
					body: page.constants.message('tags.new.error', data.id),
					report: res,
				});

				emptyAuto();
			});
		};

		//

		$scope.newNameChange = function (form) {
			if (form.newName.$dirty && form.newName.$valid) {
				updateAuto(form);
			}

			if (form.newName.$pristine || form.newName.$valid) {
				return disableNewNameError();
			}

			return enableNewNameError();
		};

		var keypress = function (event, form) {
			if (event.which == 40) {
				// down
				if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect == $scope.autoList.length - 1) {
					$scope.autoSelect = 0;
				}
				else {
					$scope.autoSelect = $scope.autoSelect + 1;
				}

				return;
			} else if (event.which == 38) {
				// up
				if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect == 0) {
					$scope.autoSelect = $scope.autoList.length - 1;
				}
				else {
					$scope.autoSelect = $scope.autoSelect - 1;
				}

				return;
			} else if (event.which == 13) {
				// enter
				if (angular.isDefined($scope.autoSelect)) {
					$scope.fillAuto(form, $scope.autoList[$scope.autoSelect]);
				}

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

		$scope.newNameBlur = function () {
			$('#newName').off('keypress');

			emptyAutoAfter(250);

			return disableNewNameError();
		};

		$scope.autoList = [];
		$scope.autoSelect = undefined;

		var updateAuto = function (form) {
			page.$http
				.get('/api/tag', {
					params: {
						query: form.newNameVal
					}
				}).success(function (data) {
					emptyAuto();

					if (form.newNameVal) {
						$scope.autoList = data;
					}
				}).error(function (errors, status) {
					$scope.messages.addError({
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
			page.$timeout(function () {
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
				$scope.messages.enable($scope.tagNewFormMessage);
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

				$scope.tagNewFormMessage = $scope.messages.add(message);
			}
		};

		var disableNewNameError = function () {
			if ($scope.tagNewFormMessage) {
				$scope.messages.disable($scope.tagNewFormMessage);
			}
		};
	}
]);
