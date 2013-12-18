define(['app/config/module'], function (module) {
	'use strict';

	module.controller('TagsPanel', ['$scope', 'Volume', 'Slot', 'Party', '$route', '$routeParams', function ($scope, Volume, Slot, Party, $route, $routeParams) {
		$scope.bootPanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.volume = Volume.get($routeParams.id, {
						id: $routeParams.id,
						tags: 'all'
					});

					$scope.$watch('volume', function () {
						$scope.automatePanel();
					}, true);
					break;

				case 'SlotView':
					$scope.slot = Slot.get($routeParams.id, {
						id: $routeParams.id,
						tags: 'all'
					});

					$scope.$watch('slot', function () {
						$scope.automatePanel();
					}, true);
					break;

				case 'PartyView':
					$scope.slot = Party.get($routeParams.id, {
						id: $routeParams.id,
						tags: 'all'
					});

					$scope.$watch('party', function () {
						$scope.automatePanel();
					}, true);
					break;
			}
		};

		//

		$scope.automatePanel = function () {
			switch ($route.current.controller) {
				case 'VolumeView':
					$scope.tags = $scope.volume.tags;
					$scope.enabled = true;
					break;

				case 'SlotView':
					$scope.tags = $scope.slot.tags;
					$scope.enabled = true;
					break;

				case 'PartyView':
					$scope.tags = $scope.party.tags;
					$scope.enabled = angular.isArray($scope.tags) && $scope.tags.length > 0;
					break;
			}
		};
	}]);
});


var tempppppppp = function (module) {
	'use strict';

	module.controller('TagsPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
		var messageTemplate = {
			type: 'alert',
			countdown: 3000
		};

		var createMessage = function (message) {
			if (typeof(message) == 'string')
				messageService.createMessage($.extend(true, {}, messageTemplate, {
					message: message
				}));
			else
				messageService.createMessage($.extend(true, {}, messageTemplate, message));
		};

		$scope.tags = $scope.tags || [
			{}
		];

		$scope.formAction = $scope.formAction || '/';

		$scope.newName = '';

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

		$scope.sortTags = function () {
			$scope.tags = $scope.tags.sort(function (a, b) {
				return (a.weight > b.weight) ? -1 : (a.weight < b.weight) ? 1 : 0;
			});
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

		$scope.voteDown = function (tag) {
			var data = {
				vote: "false",
				name: tag.name
			};

			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + tag.name + '</strong> voted down successfully!');
			});
		};

		$scope.voteNone = function (tag) {
			var data = {
				vote: "",
				name: tag.name
			};

			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + tag.name + '</strong> vote cancelled successfully!');
			});
		};

		$scope.voteUp = function (tag) {
			var data = {
				vote: "true",
				name: tag.name
			};

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

			$http.post($scope.formAction, data).success(function (tags) {
				$scope.updateTags(tags);

				createMessage('Tag <strong>' + $scope.newName + '</strong> added successfully!');

				$scope.newName = '';
			});
		};

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

		$scope.newNameChange = function () {
			if ($scope.tagNewForm.newName.$pristine || $scope.tagNewForm.newName.$valid)
				return disableNewNameError();

			return enableNewNameError();
		};

		$scope.newNameBlur = function () {
			return disableNewNameError();
		}
	}]);
};
