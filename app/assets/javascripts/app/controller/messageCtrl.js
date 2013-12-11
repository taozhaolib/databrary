define(['app/config/module'], function (module) {
	'use strict';

	module.controller('MessageCtrl', ['$scope', '$timeout', 'MessageService', function ($scope, $timeout, messageService) {
		var enabled = false;

		$scope.messages = [];
		$scope.enabledMessages = [];

		$scope.isEnabled = function () {
			return enabled;
		};

		$scope.enable = function () {
			enabled = true;
		};

		$scope.disable = function () {
			enabled = false;
		};

		$scope.findById = function (id) {
			return $.grep($scope.messages, function (i) {
				return i.id == id;
			})[0];
		};

		$scope.getIndex = function (old) {
			if (old && !old.hasOwnProperty('message'))
				old = $scope.findById(old);

			return $scope.messages.indexOf(old);
		};

		$scope.getMessage = function (old) {
			return $scope.messages[$scope.getIndex(old)];
		};

		$scope.sortMessages = function () {
			var types = messageService.getValidTypes();

			$scope.messages = $scope.messages.sort(function (a, b) {
				return (types.indexOf(a.type) < types.indexOf(b.type)) ? -1 : (types.indexOf(a.type) > types.indexOf(b.type)) ? 1 : 0;
			});
		};

		$scope.addMessage = function (message) {
			if (typeof(message) != 'undefined')
				$scope.messages.push(message);

			$scope.sortMessages();

			if (message.target)
				$scope.targetMessage(message, message.target);

			$scope.countdownMessage(message, message.countdown);

			return message;
		};

		$scope.createMessage = function (message) {
			var index = $scope.getIndex(message);

			if (~index)
				return $scope.updateMessage($scope.messages[index], message);

			return $scope.addMessage(message);
		};

		$scope.updateMessage = function (old, message) {
			var index = $scope.getIndex(old);

			if (!~index)
				return false;

			$scope.messages[index] = $.extend(true, {}, $scope.messages[index], message);

			if (message.target)
				$scope.targetMessage($scope.messages[index], message.target);

			return $scope.messages[index];
		};

		$scope.deleteMessage = function (message) {
			var index = $scope.getIndex(message);

			if (!~index)
				return false;

			$scope.countdownMessage(message, undefined);

			return $scope.messages.splice(index, 1);
		};

		$scope.enableMessage = function (message) {
			var index = $scope.getIndex(message);

			if (!~index)
				return false;

			$scope.messages[index].enabled = true;

			$scope.countdownMessage(message, message.countdown);

			return $scope.messages[index];
		};

		$scope.disableMessage = function (message) {
			var index = $scope.getIndex(message);

			if (!~index)
				return false;

			$scope.messages[index].enabled = false;

			$scope.countdownMessage(message, undefined);

			return $scope.messages[index];
		};

		var getTargetEventNames = function (target, id) {
			var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'],
				eventNamespace = '.messageTarget';

			if ($.type(id) !== 'string') id = '';

			if (focusElements.indexOf(target.prop('tagName')) >= 0)
				return ['focusin' + eventNamespace + '_' + id, 'focusout' + eventNamespace + '_' + id];
			else
				return ['mouseenter' + eventNamespace + '_' + id, 'mouseleave' + eventNamespace + '_' + id];
		};

		$scope.targetMessage = function (message, target) {
			var index = $scope.getIndex(message),
				targetEl, targetEvents;

			if (!~index)
				return false;

			if ($scope.messages[index].target) {
				targetEl = $($scope.messages[index].target);
				targetEvents = getTargetEventNames(targetEl, message.id);

				$($scope.messages[index].target).unbind(targetEvents.join(' '));
			}

			targetEl = $(target);

			if (targetEl.length === 0) {
				$scope.enableMessage(message);
				return false;
			}

			targetEvents = getTargetEventNames(targetEl, message.id);

			targetEl.bind(targetEvents[0], function () {
				$scope.$apply(function () {
					$scope.enableMessage(message);
				});
			});

			targetEl.bind(targetEvents[1], function () {
				$scope.$apply(function () {
					$scope.disableMessage(message);
				});
			});

			$scope.disableMessage(message);

			return message;
		};

		$scope.countdownMessage = function (message, countdown) {
			var index = $scope.getIndex(message);

			if (!~index)
				return false;

			if ($scope.messages[index].countdownTimer && $scope.messages[index].countdownTimer.hasOwnProperty('cancel')) {
				$scope.messages[index].countdownTimer.cancel();
			}

			if (typeof(countdown) == 'undefined')
				return false;

			if (typeof(countdown) == 'number') {
				$scope.messages[index].countdown = countdown;
				countdown = true;
			}

			if (countdown !== true || typeof($scope.messages[index].countdown) != 'number')
				return false;

			$scope.messages[index].countdownTimer = $timeout(function () {
				$scope.disableMessage($scope.messages[index]);
			}, $scope.messages[index].countdown);

			return $scope.messages[index];
		};

		$scope.updateHeight = function () {
			var $window = $(window),
				scroll = $window.scrollTop(),
				contentArea = $('#main'),
				padding = 0,
				currentPadding = parseInt(contentArea.css('padding-top'));

			var len = $scope.messages.length;

			for (var i = 0; i < len; i++) {
				if ($scope.messages[i].enabled)
					padding += $('#' + $scope.messages[i].id).outerHeight();
			}

			console.log('MessageCtrl.updateHeight() still thinks ' + padding + 'px makes any sense whatsoever.');

			contentArea.css('padding-top', padding);
			$window.scrollTop(scroll + padding - currentPadding);
		};

		$timeout(function () {
			$scope.$watch('messages', function (messages) {
				$scope.updateHeight();
			}, true);
		}, 250);

		var initialize = function () {
			messageService.registerController($scope);

			$scope.enable();
		};

		initialize();
	}]);
});
