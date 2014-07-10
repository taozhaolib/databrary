'use strict';

module.directive('panel', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs, ctrl, transclude) {
			if (angular.isDefined($attrs.id)) {
				$scope.id = $attrs.id;
				$element.addClass($attrs.id);
			} else {
				$scope.id = '';
			}

			$scope.title = $attrs.panelTitle || '';
			$scope.top = (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false;
			$scope.enabled = true;

			//

			$scope.getPanelClasses = function () {
				var classes = {};

				classes.panel = true;
				classes[$scope.id] = true;

				return classes;
			};

			//

			transclude($scope, function ($clone) {
				$element.find('[panel-body]').append($clone);
			});

			page.panels.add($scope);
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'panel.html',
			transclude: true,
			replace: true,
			priority: 100,
			link: link
		};
	}
]);
