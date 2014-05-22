module.directive('form', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if (!$attrs.name) {
				return;
			}

			var form = $scope[$attrs.name];

			switch ($attrs.messages) {
				case 'nearest':
					if ($scope.messages) {
						form.messages = $scope.messages;
					}
					break;

				case 'none':
					form.messages = page.messages.region();
					break;

				case 'default':
					form.messages = page.messages;
					break;

				default:
					if(angular.isString($attrs.messages)) {
						form.messages = page.$parse($attrs.messages)($scope);
					}

					if (!form.messages instanceof page.messages.constructor) {
						form.messages = page.messages.region();
					}
					break;
			}

			if (angular.isDefined($attrs.novalidate)) {
				var feedback = {
					server: [],
					client: [],
					tips: [],
				};

				form.validator = function (server, client, replace) {
					if (replace === true) {
						// probably have to clear bindings here too...

						if (angular.isObject(server)) {
							feedback.server = [];
						}

						if (angular.isObject(client)) {
							feedback.client = [];
						}
					}

					//

					var name;

					// server errors persist until changes
					for (name in server) {
						if (!server.hasOwnProperty(name) || !form[name]) {
							continue;
						}
					}

					// client errors persist until valid
					for (name in client) {
						if (!server.hasOwnProperty(name) || !form[name]) {
							continue;
						}
					}
				};
			}
		};

		return {
			restrict: 'E',
			link: link,
		}
	}
]);
