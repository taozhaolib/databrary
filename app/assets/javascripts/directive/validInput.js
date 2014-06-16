module.directive('validInput', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			if ($attrs.name && $attrs.validInput) {
				$element.append(page.$compile('<validator form="'+$attrs.form+'" name="'+$attrs.name+'"></validator>')($scope));
			}
		};

		var controller = function ($scope, $element, $attrs) {
			this.getIcon = function () {
				return 'valid';
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'validInput.html',
			transclude: true,
			replace: true,
			controller: controller,
			controllerAs: 'validInput',
			link: {
				pre: pre,
			},
		}
	}
]);
