define(['app/config/module'], function (module) {
	'use strict';

	module.directive('hasAccess', ['AuthService', function (authService) {
		var link = function ($scope, $element, $attrs) {
			var obj; // TODO: grab obj.permission

			var update = function () {
				if(authService.hasAccess($attrs.hasAccess, obj))
					return $element.removeClass('ng-cloak');

				return $element.addClass('ng-cloak');
			};

			//

			$scope.$on('authChange', function ($event) {
				update();
			});

			$attrs.$observe('hasAccess', function () {
				update();
			});
		};

		return {
			restrict: 'A',
			link: link
		};
	}]);
});
