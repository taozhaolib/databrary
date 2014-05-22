module.directive('messages', [
	'pageService', function (page) {
		var controller = function ($scope, $element, $attrs) {
			var Region = function () {
				$scope.messages = this;
				if ($scope[$attrs.form]) {
					$scope[$attrs.form].messages = this;
				}

				var that = this;
				this.enabled = true;

				//

				this.getRegionClasses = function () {
					var classes = [];

					if (this.enabled) {
						classes.push('messages_enabled');
					}

					if (angular.isDefined($attrs.default)) {
						classes.push('messages_default');
					} else {
						classes.push('messages_local');
					}

					return classes;
				};

				this.getMessageClasses = function (message) {
					var classes = [];

					classes.push('message');
					classes.push('message_animate');
					classes.push('message_' + message.type);

					if (message.enabled) {
						classes.push('message_enabled');
					}

					if (message.target) {
						classes.push('message_target');
					}

					if (message.closable) {
						classes.push('message_closable');
					}

					return classes;
				};

				this.getMessageStyles = function (message, $index) {
					var styles = {};

					styles['z-index'] = 500 - $index;

					return styles;
				};

				//

				this.updateHeight = function () {
					var padding = 0;

					angular.forEach(this.messages, function (message) {
						if (message.enabled) {
							padding += $('#' + message.id).outerHeight();
						}
					});

					page.$w.scrollTop(page.$w.scrollTop() + padding - parseInt(page.$m.css('padding-top')));
					page.$m.css('padding-top', padding);
				};

				$scope.$watch(function () {
					return that.messages;
				}, function () {
					that.updateHeight();
				});
			};
			Region.prototype = angular.isDefined($attrs.default) ? page.messages : page.messages.region();

			return new Region();
		};

		//

		return {
			restrict: 'E',
			replace: true,
			templateUrl: 'messages.html',
			controller: controller,
			controllerAs: 'messages',
		};
	}
]);
