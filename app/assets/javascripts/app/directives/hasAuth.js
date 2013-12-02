define([
	'app/modules/dbDirectives',
	'app/services/authService'
], function (db) {
	'use strict';

	db.directive('hasAuth', ['AuthService', function (authService) {
		var link = function ($scope, $element, $attrs) {
			var update = function () {
				if(authService.hasAuth($attrs.hasAuth))
					return $element.removeClass('ng-cloak');

				return $element.addClass('ng-cloak');
			};

			//

			$scope.$on('authChange', function ($event) {
				update();
			});

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
