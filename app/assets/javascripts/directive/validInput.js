module.directive('validInput', [
	'pageService', function (page) {
		var pre = function ($scope, $element, $attrs) {
			if ($attrs.name && $attrs.validInput) {
				$element.after(page.$compile('<validator form="'+$attrs.validInput+'" name="'+$attrs.name+'"></validator>')($scope));
			}
		};

		return {
			restrict: 'A',
			template: '<div ng-transclude></div>',
			transclude: true,
			link: {
				pre: pre,
			},
		}
	}
]);
