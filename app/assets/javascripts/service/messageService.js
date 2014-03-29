define(['config/module'], function (module) {
	'use strict';

	module.factory('MessageService', ['$rootScope', 'ArrayHelper', '$timeout', function ($rootScope, arrayHelper, $timeout) {
		var messages = arrayHelper([]);

		messages.types = ['blue', 'green', 'red', 'orange', 'yellow', 'purple'];

		//

		messages.newCatalog('id');

		messages.newTransform(function (message) {
			message.id = message.id || 'message_' + Math.random().toString(36).substring(2);
			message.type = messages.types.indexOf(message.type) != -1 ? message.type : 'blue';
			message.target = angular.isString(message.target) ? message.target : undefined;
			message.closeable = angular.isDefined(message.closeable) && message.closeable != false;
			message.countdown = parseInt(message.countdown) || false;
			message.enabled = angular.isUndefined(message.enabled) || message.enabled != false;

			message.body = message.body || undefined;

			return message;
		});

		messages.newValidate(function (message) {
			return angular.isObject(message) &&
				message.id && message.type &&
				angular.isString(message.body) &&
				message.body.length > 0 ? message : false;
		});

		messages.newOrder(function (a, b) {
			return (messages.types.indexOf(a.type) < messages.types.indexOf(b.type)) ? -1 : (messages.types.indexOf(a.type) > messages.types.indexOf(b.type)) ? 1 : 0;
		});

		//

		var addFn = messages.add;

		messages.add = function (message) {
			var newMessage = addFn(message);

			if (newMessage) {
				if (newMessage.target)
					messages.target(newMessage);

				if (newMessage.countdown)
					messages.countdown(newMessage);
			}

			return newMessage;
		};

		//

		var removeFn = messages.remove;

		messages.remove = function (message) {
			countdownUnset(message);

			return removeFn(message);
		};

		//

		var updateFn = messages.update;

		messages.update = function (message, obj) {
			var newMessage = updateFn(message, obj);

			if (newMessage)
				messages.target(newMessage);

			return newMessage;
		};

		//

		messages.enable = function (message) {
			countdownUnset(message);

			return messages.toggle(message, 'enabled', true);
		};

		messages.disable = function (message) {
			countdownUnset(message);

			return messages.toggle(message, 'enabled', false);
		};

		//

		var getTargetEvents = function (message) {
			if (!message.targetElement)
				return [];

			var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'],
				namespace = '.messageTarget';

			if (focusElements.indexOf(message.targetElement.prop('tagName')) >= 0)
				return [
						'focusin' + namespace + '_' + message.id,
						'focusout' + namespace + '_' + message.id
				];

			return [
					'mouseenter' + namespace + '_' + message.id,
					'mouseleave' + namespace + '_' + message.id
			];
		};

		messages.target = function (message, target) {
			if (messages.index(message) == -1)
				return undefined;

			if (message.targetElement) {
				message.targetElement.unbind(getTargetEvents(message).join(' '));
				delete message.targetElement;
			}

			message.target = angular.isDefined(target) ? target : message.target;

			var $target = $(message.target);

			if ($target.length === 0) {
				messages.disable(message);
				return message.target = false;
			}

			message.targetElement = $target;

			var events = getTargetEvents(message);

			$target.bind(events[0], function () {
				$rootScope.$apply(function () {
					messages.enable(message);
				});
			});

			$target.bind(events[1], function () {
				$rootScope.$apply(function () {
					messages.disable(message);
				});
			});

			messages.disable(message);

			return message;
		};

		//

		var countdownUnset = function (message) {
			if (message.countdownTimer && message.countdownTimer.hasOwnProperty('cancel'))
				message.countdownTimer.cancel();
		};

		messages.countdown = function (message, countdown) {
			if (messages.index(message) == -1)
				return undefined;

			countdownUnset(message);

			message.countdown = angular.isDefined(countdown) ? countdown : message.countdown;

			if (!angular.isNumber(message.countdown))
				return message.countdown = false;

			message.countdownTimer = $timeout(function () {
				messages.disable(message);
			}, message.countdown);

			return message;
		};

		//

		return messages;
	}]);
});
