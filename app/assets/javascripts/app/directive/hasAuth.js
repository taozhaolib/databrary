define(['app/config/module'], function (module) {
	'use strict';

	module.directive('hasAuth', ['$animate', 'AuthService', function ($animate, authService) {
		var link = function ($scope, $element, $attrs) {
			$scope.authService = authService;

			var update = function () {
				$animate[!!$scope.authService.hasAuth($attrs.hasAuth) ? 'removeClass' : 'addClass']($element, 'ng-hide');
			};

			$scope.$watch('authService.userUpdated', function () {
				update();
			}, true);

			$attrs.$observe('hasAuth', function () {
				update();
			});
		};

		return {
			restrict: 'A',
			link: link
		};
	}]);
});
