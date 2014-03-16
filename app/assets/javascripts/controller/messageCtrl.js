define(['config/module'], function (module) {
	'use strict';

	module.controller('MessageCtrl', ['$scope', '$timeout', 'MessageService', function ($scope, $timeout, messageService) {
		$scope.messages = messageService;

		$scope.enabled = true;

		//

		$scope.getControllerClasses = function () {
			var classes = [];

			if ($scope.enabled)
				classes.push('messages_enabled');

			return classes;
		};

		//

		$scope.getMessageClasses = function (message) {
			var classes = [];

			classes.push('message');
			classes.push('message_animate');
			classes.push('message_'+message.type);

			if (message.enabled)
				classes.push('message_enabled');

			if (message.target)
				classes.push('message_target');

			if (message.closable)
				classes.push('message_closable');

			return classes;
		};

		$scope.getMessageStyles = function (message, $index) {
			var styles = {};

			styles['z-index'] = 500 - $index;

			return styles;
		};

		//

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
	}]);
});
