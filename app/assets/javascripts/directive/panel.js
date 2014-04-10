define(['config/module'], function (module) {
	'use strict';

	module.directive('panel', ['panelService', function (panelService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.enabled = true;

				//

				$scope.foldPanel = function () {
					if (typeof($scope.fold) != 'undefined')
						$scope.fold();
				};

				$scope.unfoldPanel = function () {
					if (typeof($scope.unfold) != 'undefined')
						$scope.unfold();
				};

				//

				$scope.isCurrent = function () {
					var $w = $(window),
						$m = $('#main');

					var eTop = $element.offset().top,
						eBottom = eTop + $element.outerHeight(),
						pTop = $w.scrollTop() + parseFloat($m.css('margin-top'));

					return eTop - pTop <= 0 && eBottom - pTop >= 0;
				};

				$scope.getPanelClasses = function () {
					var classes = {};

					classes['panel'] = true;
					classes[$scope.panel.id] = true;

					return classes;
				};

				//

				var start = function () {
					$scope.panel = $scope;
					$scope.id = (angular.isDefined($attrs.id)) ? $attrs.id : '';
					$scope.title = $attrs.panelTitle || '';
					$scope.top = (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false;

					$scope.container = $scope;

					transclude($scope, function ($clone) {
						$element.find('[panel-body]').append($clone);
					});

					if (angular.isFunction($scope.bootPanel))
						$scope.bootPanel();

					if (angular.isFunction($scope.refreshPanel))
						$scope.refreshPanel();

					panelService.add($scope);
				};

				start();
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'panel.html',
			transclude: true,
			replace: true,
			priority: 100,
			compile: compile
		};
	}]);
});
