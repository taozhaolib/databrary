module.directive('form', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			if (!$attrs.name) {
				return;
			}

			var form = $scope[$attrs.name];

			form.$element = $element;

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
				form.validators = {};
				form.validator = function (server, client, replace) {
					var name, target;

					for (name in server) {
						if (server.hasOwnProperty(name) && form.validator[name]) {
							form.validators[name].server(server[name], replace);
						}
					}

					for (name in client) {
						if (server.hasOwnProperty(name) && form.validator[name]) {
							form.validators[name].client(client[name], replace);
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
