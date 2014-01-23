define(['app/config/module'], function (module) {
	'use strict';

	module.controller('TagsPanel', ['$scope', 'Tag', '$route', 'MessageService', function ($scope, Tag, $route, messageService) {
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
					$scope.target.segment = "-";
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

		//

		$scope.voteDown = function (name) {
			var data = {
				vote: "false",
				name: name
			};

			Tag.update({
				vote: "false",
				name: name,
				container: $scope.target.container,
				segment: $scope.target.segment
			}, function (data) {

			});


			// NOT IT!
			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + tag.name + '</strong> voted down successfully!');
			});
		};

		$scope.voteNone = function (name) {
			var data = {
				vote: "",
				name: name
			};

			// NOT IT!
			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + tag.name + '</strong> vote cancelled successfully!');
			});
		};

		$scope.voteUp = function (name) {
			var data = {
				vote: "true",
				name: name
			};

			// NOT IT!
			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + tag.name + '</strong> voted up successfully!');
			});
		};

		$scope.voteNew = function () {
			var data = {
				vote: "true",
				name: $scope.newName
			};

			if ($scope.tagNewForm.$invalid)
				return;

			// NOT IT!
			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + $scope.newName + '</strong> added successfully!');

				$scope.newName = '';
			});
		};

		//

		$scope.newName = '';

		$scope.newNameChange = function () {
			if ($scope.tagNewForm.newName.$pristine || $scope.tagNewForm.newName.$valid)
				return disableNewNameError();

			return enableNewNameError();
		};

		$scope.newNameBlur = function () {
			return disableNewNameError();
		};

		//

		var enableNewNameError = function () {
			if ($scope.tagNewForm.newName.message) {
				$scope.tagNewForm.newName.message = messageService.updateMessage($scope.tagNewForm.newName.message, {enabled: true});
			} else {
				var message = {
					enabled: true,
					type: 'error',
					message: '<dl>' +
						'<dt>Tag Name</dt>' +
						'<dd>Must be between 3 and 32 characters.</dd>' +
						'<dd>Only letters, spaces, and dashes (-) allowed.</dd>' +
						'</dl>'
				};

				$scope.tagNewForm.newName.message = messageService.createMessage(message);
			}
		};

		var disableNewNameError = function () {
			if ($scope.tagNewForm.newName.message)
				$scope.tagNewForm.newName.message = messageService.updateMessage($scope.tagNewForm.newName.message, {enabled: false});
		};


	}]);
});


var tempppppppp = function (module) {
	'use strict';

	module.controller('TagsPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {

		$scope.formAction = $scope.formAction || '/';

		$scope.getIndex = function (tag) {
			return $scope.tags.indexOf(tag);
		};

		$scope.getTag = function (tag) {
			return $scope.tags[$scope.getIndex(tag)];
		};

		$scope.createTag = function (tag) {
			$scope.tags.push(tag);

			return $scope.tags.slice(-1)[0];
		};

		$scope.updateTags = function (tags) {
			if (typeof(tags) != 'undefined')
				$scope.tags = tags;

			$scope.sortTags();
		};

		$scope.updateTag = function (old, tag) {
			var index = $scope.getIndex(old);

			if (!~index)
				return false;

			$scope.tags[index] = $.extend(true, {}, $scope.tags[index], tag);

			return $scope.tags[index];
		};

		$scope.deleteTag = function (tag) {
			var index = $scope.getIndex(tag);

			if (!~index)
				return false;

			return $scope.tags.splice(index, 1);
		};
	}]);
};
