// Module houses everything databrary

var dbModule = angular.module('DatabraryModule', ['ngSanitize', 'ngAnimate']);

dbModule.run(function ($rootScope, $location, $compile) {
	// init
});

//

dbModule.directive('dbModeClient', function () {
	var compile = function ($element) {
		$element.replaceWith($element.html());
	};

	return {
		restrict: 'A',
		compile: compile
	};
});

dbModule.directive('dbModeServer', function () {
	var compile = function ($element) {
		$element.remove();
	};

	return {
		restrict: 'A',
		compile: compile
	};
});

//

dbModule.directive('dbCarousel', function ($timeout) {
	var link = function ($scope, $element) {
		var pauseTime = 5000,
			fadeTime = 1000,
			timeout;

		$scope.update = function (reverse) {
			if (reverse !== true) {
				$element.children().last().fadeOut(fadeTime, function () {
					$(this).prependTo($element).fadeIn(fadeTime);
				});
			} else {
				$element.children().first().fadeOut(0, function () {
					$(this).appendTo($element).fadeIn(fadeTime);
				});
			}
		};

		$scope.schedule = function (pause) {
			timeout = $timeout(function () {
				$scope.update(true);
				$scope.schedule(pause);
			}, pause);
		};

		$element.on('$destroy', function () {
			$timeout.cancel(timeout);
		});

		$scope.schedule(pauseTime);
	};

	return {
		restrict: 'A',
		link: link
	};
});

//

dbModule.directive('dbFold', function () {
	var foldableClass = 'foldable',
		folderClass = 'folder',
		foldClass = 'fold',
		currentlyClass = 'folded',
		slideTime = 500;

	var link = function ($scope, $element, $attrs) {
		var folder = $element.find('[db-fold-folder]'),
			fold = $element.find('[db-fold-folded]');

		$element.addClass(foldableClass);
		folder.addClass(folderClass);
		fold.addClass(foldClass);

		$element.on('$destroy', function () {
			$element.removeClass(foldableClass + ' ' + currentlyClass);
			folder.removeClass(folderClass);
			fold.removeClass(foldClass);
		});

		$scope.hide = function () {
			$element.addClass(currentlyClass);
			fold.slideUp(slideTime);
		};

		$scope.show = function () {
			$element.removeClass(currentlyClass);
			fold.slideDown(slideTime);
		};

		$scope.toggle = function () {
			$scope.currently = !$scope.currently;
		};

		$scope.$watch('currently', function (currently) {
			if (currently) {
				$scope.hide();
			} else {
				$scope.show();
			}
		});

		$scope.currently = $attrs.dbFoldCurrently == "true";
		$element.removeAttr('db-fold-currently');
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	}
});

//

dbModule.directive('dbBaseline', function ($timeout) {
	var base = 6,
		pauseTime = 500;

	var link = function ($scope, $element, $attrs) {
		var ratio,
			timeout;

		$scope.updateRatio = function (val) {
			if ($.isNumeric(val))
				ratio = parseFloat(val);
			else
				switch (val) {
					case 'wide':
					case '16x9':
						ratio = .5625;
						break;

					case 'tube':
					case '4x3':
						ratio = .75;
						break;

					case 'square':
					case '1x1':
					default:
						ratio = 1;
						break;
				}
		};

		$scope.trigger = function () {
			var width = $element.outerWidth(false),
				height;

			height = width * ratio;
			height = height - (height % base);

			if (height > 0)
				$element.height(height);
			else
				$element.css('height', '');
		};

		$(window).on('resize', function () {
			clearTimeout(timeout);

			timeout = setTimeout(function () {
				$scope.trigger();
			}, pauseTime);
		});

		$element.on('$destroy', function () {
			$timeout.cancel(timeout);
		});

		$scope.updateRatio($attrs.dbBaselineRatio);
		$element.removeAttr('db-baseline-ratio');

		$scope.trigger();
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	};
});

//

dbModule.directive('dbHover', function ($timeout) {
	var hoverableClass = 'hoverable',
		currentlyClass = 'hovered',
		hoverWrap = $('<div class="hover_wrap" style="position: relative;"></div>'),
		pauseTime = 0,
		fadeTime = 150;

	var link = function ($scope, $element) {
		var timeout, clone;

		$element.wrap(hoverWrap.clone());

		$scope.show = function () {
			var position = $element.position();

			clone = $element.clone();

			clone.hide().addClass(currentlyClass).css({
				'z-index': 750,
				'position': 'absolute',
				'left': position.left,
				'top': position.top,
				'width': '100%'
			});

			$element.after(clone);
			clone.fadeIn(fadeTime);
		};

		$scope.hide = function () {
			clone.fadeOut(fadeTime, function () {
				clone.off('mouseleave');

				clone.remove();

				clone = undefined;
			});
		};

		$element.on('mouseenter', function () {
			clearTimeout(timeout);

			$scope.show();

			clone.on('mouseleave', function () {
				clearTimeout(timeout);

				timeout = setTimeout(function () {
					$scope.hide();
				}, pauseTime);
			});
		});

		$(window).on('resize scroll', function () {
			if (typeof(clone) != 'undefined')
				$scope.hide();
		});

		$element.on('$destroy', function () {
			$timeout.cancel(timeout);
			$element.removeClass(hoverableClass);
		});

		$element.addClass(hoverableClass);
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	};
});

//

dbModule.factory('MessageService', function ($rootScope) {
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

		if (typeof(attr) != 'number')
			return def;

		return attr;
	};

	messageService.getValidType = function (type) {
		if ($.inArray(type, validTypes) >= 0)
			return type;

		return 'alert';
	};

	messageService.getValidTypes = function () {
		return validTypes;
	};

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
		message.countdown = attrToInt(messageAttrs.dbMessageCountdown, message.closeable);
		message.enabled = attrToBoolean(messageAttrs.dbMessageEnabled, message.enabled);
		message.message = messageAttrs.dbMessageMessage || messageElement.html();

		if (!message.message)
			return false;

		return message;
	};

	messageService.formatMessage = function (message) {
		return $.extend(true, {}, messageTemplate, message);
	};

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

	return messageService;
});

dbModule.controller('MessageCtrl', ['$scope', '$timeout', 'MessageService', function ($scope, $timeout, messageService) {
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

		if (!targetEl.exists) {
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

		if ($scope.messages[index].countdownTimer) {
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

dbModule.directive('dbMessage', ['MessageService', function (messageService) {
	var link = function ($scope, $element, $attrs) {
		messageService.postRawMessage($scope, $element, $attrs);

		$element.remove();
	};

	return {
		restrict: 'A',
		link: link
	}
}]);

//

dbModule.directive('dbFormRepeater', function () {
	var link = function ($scope, $element, $attrs) {
		$scope.repeats = $scope.repeats || [
			{}
		];

		$scope.getIndex = function (repeat) {
			return $scope.repeats.indexOf(repeat);
		};

		$scope.getRepeat = function (repeat) {
			return $scope.repeats[$scope.getIndex(repeat)];
		};

		$scope.createRepeat = function () {
			$scope.repeats.push({});

			return $scope.repeats.slice(-1)[0];
		};

		$scope.updateRepeat = function (old, repeat) {
			var index = $scope.getIndex(old);

			if (!~index)
				return false;

			$scope.repeats[index] = $.extend(true, {}, $scope.repeats[index], repeat);

			return $scope.repeats[index];
		};

		$scope.deleteRepeat = function (repeat) {
			var index = $scope.getIndex(repeat);

			if (!~index)
				return false;

			var deleted = $scope.repeats.splice(index, 1);

			if ($scope.repeats.length == 0)
				$scope.repeats.push({});

			return deleted;
		};

		$scope.isMoveable = function () {
			return false;
		}
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	}
});

//

dbModule.controller('TagsPanelCtrl', ['$scope', '$http', 'MessageService', function ($scope, $http, messageService) {
	var messageTemplate = {
		type: 'alert',
		countdown: 3000
	};

	var createMessage = function (message) {
		messageService.createMessage($.extend(true, {}, messageTemplate, {
			message: message
		}));
	};

	$scope.tags = $scope.tags || [
		{}
	];

	$scope.formAction = $scope.formAction || '/';

	$scope.newName = "";

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
			return (a.weight < b.weight) ? -1 : (a.weight > b.weight) ? 1 : 0;
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

			createMessage('Tag <strong>' + tag.name + '</strong> added successfully!');

			$scope.newName = "";
		});
	};
}]);

//

