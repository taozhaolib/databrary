'use strict';

module.directive('tab', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attr, tabset) {
			$scope.id = $attr.id;
			$scope.name = $attr.name;
			$scope.active = false;

			$scope.enabled = !angular.isString($attr.access) ? true :
				page.auth.hasAccess($attr.access, $scope.party ? $scope.party : $scope.volume);

			//

			tabset.addTab($scope);

			//

			$scope.tabClass = function () {
				var cls = [];

				if ($scope.active) {
					cls.push('active');
				}

				return cls;
			};
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
	}
]);
