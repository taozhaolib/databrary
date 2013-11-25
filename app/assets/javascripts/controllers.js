// Module houses everything databrary

var dbModule = angular.module('DatabraryModule', ['ngSanitize', 'ngAnimate', 'ngStorage']);

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

dbModule.directive('dbFold', ['$sessionStorage', function ($sessionStorage) {
	var foldableClass = 'foldable',
		folderClass = 'folder',
		foldClass = 'fold',
		foldedClass = 'folded',
		folderAttr = '[db-fold-folder]',
		foldAttr = '[db-fold-folded]',
		slideTime = 500;

	var link = function ($scope, $element, $attrs) {
		$scope.$storage = $sessionStorage;

		$scope.id = $element.attr('id') || 'unknown';

		$element.addClass(foldableClass);
		$element.find(folderAttr).addClass(folderClass);
		$element.find(foldAttr).addClass(foldClass);

		$element.on('$destroy', function () {
			$element.removeClass(foldableClass + ' ' + foldedClass);
			$element.find(folderAttr).removeClass(folderClass);
			$element.find(folderAttr).removeClass(foldClass);
		});

		//

		$scope.isFoldable = function () {
			return true;
		};

		//

		$scope.foldUp = function () {
			$scope.isFolded = true;
		};

		$scope.foldDown = function () {
			$scope.isFolded = false;
		};

		$scope.foldToggle = function () {
			if ($scope.isFolded)
				$scope.foldDown();
			else
				$scope.foldUp();
		};

		//

		$scope.setFolding = function () {
			if($attrs.dbFoldForget)
				return undefined;

			$scope.$storage['folding_' + $scope.id] = $scope.isFolded;
		};

		$scope.getFolding = function () {
			if ($attrs.dbFoldForget || typeof($scope.$storage['folding_' + $scope.id]) == 'undefined')
				return undefined;

			return $scope.$storage['folding_' + $scope.id];
		};

		$scope.restoreFolding = function () {

			var isFolded = $scope.getFolding();

			if (typeof(isFolded) == 'undefined')
				$scope.isFolded = $attrs.dbFoldCurrently == "true";
			else
				$scope.isFolded = isFolded;

			$element.removeAttr('db-fold-currently');
		};

		//

		$scope.$watch('isFolded', function () {
			$scope.setFolding();
		});

		//

		$scope.restoreFolding();
	};

	return {
		restrict: 'A',
		scope: true,
		link: link
	}
}]);

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

//

dbModule.factory('PanelsService', function ($rootScope) {
	var panelsService = {},
		panelsCtrl;

	//

	panelsService.setController = function (controller) {
		panelsCtrl = controller;
	};

	panelsService.getController = function () {
		return panelsCtrl;
	};

	//

	return panelsService;
});

dbModule.controller('PanelsCtrl', ['$scope', '$sessionStorage', 'PanelsService', function ($scope, $sessionStorage, panelsService) {
	$scope.$storage = $sessionStorage;

	panelsService.setController($scope);

	//

	$scope.panels = {};

	$scope.getPanelId = function (panel) {
		var id = (typeof(panel) == 'object') ? panel.$id : panel;

		if ($scope.panels[id])
			return id;

		return false;
	};

	$scope.getPanel = function (panel) {
		var id = (typeof(panel) == 'object') ? panel.$id : panel;

		if ($scope.panels[id])
			return $scope.panels[id];

		return false;
	};

	//

	$scope.addPanel = function (panel) {
		$scope.panels[panel.$id] = panel;
	};

	$scope.createPanel = function (panel) {
		var id = $scope.getPanelId(panel);

		if (id)
			return $scope.updatePanel(panel);

		return $scope.addPanel(panel);
	};

	$scope.updatePanel = function (panel) {
		var id = $scope.getPanelId(panel);

		if (id)
			return $scope.panels[id] = $.extend(true, {}, $scope.panels[id], panel);

		return false;
	};

	$scope.deletePanel = function (panel) {
		var id = $scope.getPanelId(panel),
			old = $scope.panels[id];

		if (old && delete $scope.panels[id])
			return old;

		return false;
	};

	//

	$scope.$watch(function () {
		var list = '';

		for (var id in $scope.panels)
			list += $scope.panels[id].isFolded;

		return list;
	}, function (newValue, oldValue, scope) {

	}, true);
}]);

dbModule.directive('dbPanel', ['PanelsService', function (panelsService) {
	var ps = panelsService;

	var link = function ($scope, $element, $attrs) {
		var panelsCtrl = panelsService.getController();

		$scope.isEnabled = $attrs.dbPanelEnabled != "false";
		$element.removeAttr('db-panel-enabled');

		$scope.id = $element.attr('id');

		//

		$scope.panelEnable = function () {
			$scope.isEnabled = true;
		};

		$scope.panelDisable = function () {
			$scope.isEnabled = false;
		};

		$scope.panelToggle = function () {
			if ($scope.isEnabled)
				$scope.panelEnable();
			else
				$scope.panelDisable();
		};

		//

		panelsCtrl.createPanel($scope);
	};

	return {
		restrict: 'A',
		scope: true,
		priority: 100,
		link: link
	};
}]);