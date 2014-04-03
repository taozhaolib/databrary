define(['config/module'], function (module) {
	'use strict';

	module.directive('tab', ['AuthService', function (auth) {
		var link = function ($scope, $el, $attr, tabset, transclude) {
			$scope.id = $attr.id;
			$scope.name = $attr.name;
			$scope.active = false;

			$scope.enabled = !angular.isString($attr.access) ? true :
				auth.hasAccess($attr.access, $scope.party ? $scope.party : $scope.volume);

			//

			tabset.addTab($scope);

			//

			$scope.tabClass = function () {
				var cls = [];

				if ($scope.active)
					cls.push('active');

				return cls;
			};

			//

			transclude($scope, function ($clone) {
				$el.find('[tab-body]').append($clone);
			});
		};

		return {
			restrict: 'E',
			templateUrl: 'tab.html',
			replace: true,
			transclude: true,
			scope: true,
			require: '^tabset',
			link: link
		};
	}]);
});
