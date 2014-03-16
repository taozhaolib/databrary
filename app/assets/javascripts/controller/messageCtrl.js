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
			classes.push('message_' + message.type);

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
				$main = $('#main');

			var padding = 0;

			angular.forEach($scope.messages, function (message) {
				if (message.enabled)
					padding += $('#' + message.id).outerHeight();
			});

			$window.scrollTop($window.scrollTop() + padding - parseInt($main.css('padding-top')));
			$main.css('padding-top', padding);
		};

		$scope.$watch('messages', function (messages) {
			$scope.updateHeight();
		});
	}]);
});
