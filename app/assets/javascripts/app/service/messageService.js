define(['app/config/module'], function (module) {
	'use strict';

	module.factory('MessageService', ['$rootScope', function ($rootScope) {
		var messageService = {},
			messageCtrl, queue = [];

		var validTypes = ['alert', 'error', 'info', 'trace'];

		var messageTemplate = {
			id: undefined,
			type: undefined,
			target: false,
			closeable: false,
			countdown: undefined,
			enabled: true,
			message: undefined
		};

		var parseBoolean = function (val) {
			if ($.isNumeric(val))
				return !!(parseFloat(val) > 0);

			return typeof(val) == 'string' && val != 'false';
		};

		var attrToBoolean = function (attr, def) {
			if (typeof(attr) == 'undefined')
				return def;

			return parseBoolean(attr);
		};

		var attrToInt = function (attr, def) {
			attr = parseInt(attr);

			if (isNaN || typeof(attr) != 'number')
				return def;

			return attr;
		};

		//

		messageService.getValidType = function (type) {
			if ($.inArray(type, validTypes) >= 0)
				return type;

			return 'alert';
		};

		messageService.getValidTypes = function () {
			return validTypes;
		};

		//

		messageService.registerController = function (controller) {
			messageCtrl = controller;
			messageService.processQueue();
		};

		messageService.processQueue = function () {
			var q;

			while (queue.length) {
				q = queue.shift();

				switch (q.length) {
					case 2:
						messageService[q[0]](q[1]);
						break;

					case 3:
						messageService[q[0]](q[1], q[2]);
						break;
				}
			}
		};

		//

		messageService.postRawMessage = function (messageScope, messageElement, messageAttrs) {
			var message = messageService.formatRawMessage(messageScope, messageElement, messageAttrs);

			if (!message)
				return false;

			return messageService.createMessage(message);
		};

		messageService.formatRawMessage = function (messageScope, messageElement, messageAttrs) {
			var message = $.extend(true, {}, messageTemplate);

			message.id = messageElement.attr('id') || 'message_' + Math.random().toString(36).substring(2);
			message.type = messageService.getValidType(messageAttrs.dbMessageType);
			message.target = messageAttrs.dbMessageTarget || message.target;
			message.closeable = attrToBoolean(messageAttrs.dbMessageCloseable, message.closeable);
			message.countdown = attrToInt(messageAttrs.dbMessageCountdown, message.countdown);
			message.enabled = attrToBoolean(messageAttrs.dbMessageEnabled, message.enabled);
			message.message = messageAttrs.dbMessageMessage || messageElement.html();

			if (!message.message)
				return false;

			return message;
		};

		messageService.formatMessage = function (message) {

			message.id = message.id || 'message_' + Math.random().toString(36).substring(2);

			return $.extend(true, {}, messageTemplate, message);
		};

		//

		messageService.createMessage = function (message) {
			message = messageService.formatMessage(message);

			if (typeof(messageCtrl) == 'undefined') {
				queue.push(['createMessage', message]);
				return undefined;
			}

			return messageCtrl.createMessage(message);
		};

		messageService.updateMessage = function (old, message) {
			if (typeof(messageCtrl) == 'undefined') {
				queue.push(['updateMessage', old, message]);
				return undefined;
			}

			return messageCtrl.updateMessage(old, message);
		};

		messageService.deleteMessage = function (old) {
			if (typeof(messageCtrl) == 'undefined') {
				queue.push(['deleteMessage', old]);
				return undefined;
			}

			return messageCtrl.deleteMessage(old);
		};

		messageService.getMessage = function (old) {
			if (typeof(messageCtrl) == 'undefined') {
				queue.push(['getMessage', old]);
				return undefined;
			}

			return messageCtrl.getMessage(old);
		};

		//

		return messageService;
	}]);
});
