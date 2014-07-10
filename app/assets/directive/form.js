'use strict';

module.directive('form', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			if (!$attrs.name) {
				return;
			}

			var form = $scope[$attrs.name];
			var unclaimed = {};

			form.$element = $element;

			if (angular.isDefined($attrs.novalidate)) {
				form.validators = {};
				form.validator = {
					server: function (res, replace) {
						if ($.isEmptyObject(res)) {
							res.data = {};
						} else if (!angular.isObject(res.data)) {
							form.messages.addError({
								body: page.constants.message('error.generic'),
								report: res,
							});
						}

						for (var name in form.validators) {
							if (form.validators.hasOwnProperty(name)) {
								form.validators[name].server(res.data[name] || {}, replace);
							} else if (form.messages) {
								form.messages.add({
									type: 'red',
									closeable: true,
									body: angular.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
								});
							}
						}

						for (var name in res.data) {
							if (res.data.hasOwnProperty(name) && form.validators[name]) {
								form.validators[name].server(res.data[name], replace);
							} else if (form.messages) {
								form.messages.add({
									type: 'red',
									closeable: true,
									body: angular.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
								});
							}
						}
					},

					clearServer: function () {
						angular.forEach(form.validators, function (validator) {
							validator.server({}, true);
						});
					},

					client: function (data, replace) {
						for (var name in data) {
							if (!data.hasOwnProperty(name)) {
								continue;
							} else if (form.validators[name]) {
								form.validators[name].client(data[name], replace);
							} else {
								unclaimed[name] = data[name];
							}
						}
					},

					add: function (name, validator) {
						form.validators[name] = validator;

						if (unclaimed[name]) {
							validator.client(unclaimed[name], true);
							delete unclaimed[name];
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
					if (angular.isString($attrs.messages)) {
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
