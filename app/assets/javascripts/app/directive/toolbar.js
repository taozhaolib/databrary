define(['app/config/module'], function (module) {
	'use strict';

	module.directive('toolbar', ['$location', '$anchorScroll', '$timeout', 'EventService', 'AuthService', 'PanelService', function ($location, $anchorScroll, $timeout, eventService, authService, panelService) {
		var controller = function ($scope, $element, $attrs) {
			$scope.auth = $scope.auth || authService;

			//

			$scope.panels = panelService;

			$scope.focusPanel = function (panel) {
				panelService.focus(panel);
			};

			$scope.getPanelClasses = function (panel) {
				return {
					'current': $scope.checkCurrentPanel(panel)
				};
			};

			//

			var $w = $(window);

			var currentPanel;

			$scope.checkCurrentPanel = function (panel) {
				return currentPanel == panel;
			};

			$scope.updateCurrentPanel = function () {
				for (var i = $scope.panels.length - 1; i >= 0; i--) {
					if ($scope.panels[i].isCurrent()) {
						currentPanel = $scope.panels[i];
						break;
					}
				}
			};

			//

			$scope.$watch(function () {
				return $w.scrollTop() + ' ' + $w.scrollLeft() + ' ' + $w.height() + ' ' + $w.width();
			}, function () {
				$scope.updateCurrentPanel();
			});

			$w.on('scroll resize', function () {
				$scope.$apply(function () { $scope.updateCurrentPanel(); });
			});

			//

			$scope.links = [];
		};

		return {
			restrict: 'A',
			templateUrl: 'toolbar.html',
			replace: true,
			controller: controller
		};
	}]);
});
