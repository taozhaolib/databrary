module.directive('form', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			if (!$attrs.name) {
				return;
			}

			var form = $scope[$attrs.name];

			form.$element = $element;

			if (angular.isDefined($attrs.novalidate)) {
				form.validators = {};
				form.validator = {
					server: function (data, replace) {
						for (var name in data) {
							if (data.hasOwnProperty(name) && form.validator[name]) {
								form.validators[name].server(data[name], replace);
							}
						}
					},

					client: function (data, replace) {
						for (var name in data) {
							if (data.hasOwnProperty(name) && form.validators[name]) {
								form.validators[name].client(data[name], replace);
							}
						}
					},
				};
			}
		};

		var post = function ($scope, $element, $attrs) {
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
		};

		return {
			restrict: 'E',
			link: {
				pre: pre,
				post: post,
			},
		}
	}
]);
