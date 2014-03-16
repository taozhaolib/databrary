define(['app/config/module'], function (module) {
	'use strict';

	module.controller('TagsPanel', ['$scope', 'Tag', '$route', 'MessageService', 'Volume', function ($scope, Tag, $route, messageService, Volume) {
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
					break;

				case 'SlotView':
					$scope.prepareTags($scope.slot.tags);
//					$scope.target.container = null;
//					$scope.target.segment = null;
					$scope.enabled = true;
					break;

				case 'PartyView':
					$scope.prepareTags($scope.party.tags);
//					$scope.target.container = null;
//					$scope.target.segment = null;
					$scope.enabled = angular.isArray($scope.tags) && $scope.tags.length > 0;
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
					Volume.get({
						id: $scope.volume.id,
						tags: ''
					}, function (data) {
						$scope.volume.tags = data.tags;
						$scope.refreshPanel();
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
			}, function (newTag, status, headers, config) {
//				$scope.tags.splice($scope.tags.indexOf(tag), 1, newTag); // currently returns slot

				switch(vote) {
					case -1:
						createMessage('Tag <strong>' + tag.id + '</strong> voted down successfully!');
						tag.weight = tag.vote ? tag.weight -2 : tag.weight - 1;
						tag.vote = -1;
						break;

					case 0:
						createMessage('Tag <strong>' + tag.id + '</strong> vote cancelled successfully!');
						tag.weight = tag.weight - tag.vote;
						delete tag.vote;
						break;

					case 1:
						createMessage('Tag <strong>' + tag.id + '</strong> voted up successfully!');
						tag.weight = tag.vote ? tag.weight + 2 : tag.weight + 1;
						tag.vote = 1;
						break;
				}
			});
		};

		$scope.voteNew = function (form) {
			if (form.$invalid)
				return;

			var tagModel = new Tag({id: $scope.newName});

			tagModel.$save({
				id: $scope.newName,
				vote: "true",
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (newTag, status, headers, config) {
				createMessage('Tag <strong>' + $scope.newName + '</strong> added successfully!');

				$scope.newName = '';

				$scope.retrieveTags();
			});
		};

		//

		$scope.newName = '';

		$scope.newNameChange = function (form) {
			if (form.newName.$pristine || form.newName.$valid)
				return disableNewNameError();

			return enableNewNameError();
		};

		$scope.newNameBlur = function (form) {
			return disableNewNameError();
		};

		//

		var enableNewNameError = function () {
			if ($scope.tagNewFormMessage) {
				messageService.enable($scope.tagNewFormMessage);
			} else {
				var message = {
					enabled: true,
					type: 'error',
					body: '<dl>' +
						'<dt>Tag Name</dt>' +
						'<dd>Must be between 3 and 32 characters.</dd>' +
						'<dd>Only letters, spaces, and dashes (-) allowed.</dd>' +
						'</dl>'
				};

				$scope.tagNewFormMessage = messageService.add(message);
			}
		};

		var disableNewNameError = function () {
			if ($scope.tagNewFormMessage)
				messageService.disable($scope.tagNewFormMessage);
		};


	}]);
});
