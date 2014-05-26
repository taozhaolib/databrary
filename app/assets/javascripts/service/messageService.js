module.factory('messageService', [
	'$rootScope',
	'ArrayHelper',
	'$timeout',
	'constantService',
	function ($rootScope, ArrayHelper, $timeout, constants) {
		var types = ['blue', 'green', 'red', 'orange', 'yellow', 'purple'];

		//

		var transformFn = function (message) {
			message.id = message.id || 'message_' + Math.random().toString(36).substring(2);
			message.type = types.indexOf(message.type) != -1 ? message.type : 'blue';
			message.target = angular.isString(message.target) ? message.target : undefined;
			message.closeable = angular.isDefined(message.closeable) && message.closeable != false;
			message.countdown = parseInt(message.countdown) || false;
			message.enabled = angular.isUndefined(message.enabled) || message.enabled != false;

			message.body = message.body || undefined;

			return message;
		};

		var validateFn = function (message) {
			return angular.isObject(message) &&
				message.id && message.type &&
				angular.isDefined(message.body) &&
				message.body.length > 0 ? message : false;
		};

		var orderFn = function (a, b) {
			return (types.indexOf(a.type) < types.indexOf(b.type)) ? -1 : (types.indexOf(a.type) > types.indexOf(b.type)) ? 1 : 0;
		};

		//

		var register = function (message) {
			if (message) {
				if (message.target) {
					this.target(message);
				}

				if (message.countdown) {
					this.countdown(message);
				}
			}

			return message;
		};

		var errorHTML = function (html) {
			return function () {
				var doc = document.open('text/html', 'replace');
				doc.write(html);
				doc.close();
			}
		};

		var getTargetEvents = function (message) {
			if (!message.targetElement) {
				return [];
			}

			var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'],
				namespace = '.messageTarget';

			if (focusElements.indexOf(message.targetElement.prop('tagName')) >= 0) {
				return [
						'focusin' + namespace + '_' + message.id,
						'focusout' + namespace + '_' + message.id
				];
			}

			return [
					'mouseenter' + namespace + '_' + message.id,
					'mouseleave' + namespace + '_' + message.id
			];
		};

		var countdownUnset = function (message) {
			if (message.countdownTimer) {
				$timeout.cancel(message.countdownTimer);
			}
		};

		//

		var MessageService = function () {
			this.newCatalog('id');
			this.newTransform(transformFn);
			this.newValidate(validateFn);
			this.newOrder(orderFn);
		};
		MessageService.prototype = new ArrayHelper();

		//

		MessageService.prototype.region = function () {
			return new MessageService();
		};

		//

		//

		MessageService.prototype.add = function (message) {
			var newMessage = ArrayHelper.prototype.add.call(this, message);

			register.call(this, newMessage);

			return newMessage;
		};

		MessageService.prototype.addError = function (message) {
			message.countdown = undefined;
			message.closeable = true;
			message.type = 'red';

			var newMessage = ArrayHelper.prototype.add.call(this, message);

			if (!newMessage) {
				return false;
			}

			newMessage.body = constants.message('error.prefix') + ' ' + newMessage.body;

			if (message.report) {
				message.errors = message.report.data;
				message.status = message.report.status;
				message.url = message.report.config.url;
			}

			if (!message.errors) {
				newMessage.body = newMessage.body + ' ' + constants.message('error.suffix');
			} else if (angular.isString(message.errors)) {
				newMessage.fn = errorHTML(message.errors);
				newMessage.body = newMessage.body + ' ' + constants.message('error.view');
			} else if (angular.isObject(message.errors)) {
				var moreBody = '';
				var messageBody = '';

				angular.forEach(message.errors, function (errorArray, field) {
					moreBody += '<dt>' + (field || '') + '</dt><dd>' + errorArray.join('</dd><dd>') + '</dd>';
					messageBody += 'Field "' + (field || 'validation') + '":\n' + errorArray.join('\n') + '\n\n';
				});

				if (message.status) {
					messageBody = 'Status:\n' + message.status + '\n\n' + messageBody;
				}

				if (messageBody) {
					newMessage.body = newMessage.body + ' ' + constants.message('error.report', encodeURIComponent(constants.message('error.report.subject', message.status || 'Unknown', message.url || 'Location unknown')), encodeURIComponent(constants.message('error.report.body', messageBody))) + moreBody;
				}
			}

			delete message.report;
			delete message.errors;
			delete message.status;
			delete message.url;

			register.call(this, newMessage);
			return newMessage;
		};

		//

		MessageService.prototype.remove = function (message) {
			countdownUnset(message);
			return ArrayHelper.prototype.remove.call(this, message);
		};

		//

		MessageService.prototype.update = function (message, obj) {
			var newMessage = ArrayHelper.prototype.update.call(this, message, obj);

			if (newMessage) {
				this.target(newMessage);
			}

			return newMessage;
		};

		//

		MessageService.prototype.enable = function (message) {
			countdownUnset(message);
			return this.toggle(message, 'enabled', true);
		};

		MessageService.prototype.disable = function (message) {
			countdownUnset(message);
			return this.toggle(message, 'enabled', false);
		};

		//

		MessageService.prototype.target = function (message, target) {
			var that = this;

			if (this.index(message) == -1) {
				return undefined;
			}

			if (message.targetElement) {
				message.targetElement.unbind(getTargetEvents(message).join(' '));
				delete message.targetElement;
			}

			message.target = angular.isDefined(target) ? target : message.target;

			var $target = $(message.target);

			if ($target.length === 0) {
				this.disable(message);
				return message.target = false;
			}

			message.targetElement = $target;

			var events = getTargetEvents(message);

			$target.bind(events[0], function () {
				$rootScope.$apply(function () {
					that.enable(message);
				});
			});

			$target.bind(events[1], function () {
				$rootScope.$apply(function () {
					that.disable(message);
				});
			});

			this.disable(message);

			return message;
		};

		//

		MessageService.prototype.countdown = function (message, countdown) {
			var that = this;

			if (this.index(message) == -1) {
				return undefined;
			}

			countdownUnset(message);

			message.countdown = angular.isDefined(countdown) ? countdown : message.countdown;

			if (!angular.isNumber(message.countdown)) {
				return message.countdown = false;
			}

			message.countdownTimer = $timeout(function () {
				that.disable(message);
			}, message.countdown);

			return message;
		};

		//

		return new MessageService();
	}
]);
