define(['config/module'], function (module) {
	'use strict';

	module.directive('toolbar', ['$location', '$timeout', 'authService', 'panelService', 'pageService', function ($location, $timeout, authService, panelService, page) {
		var controller = function ($scope) {
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

			$w.on('scroll resize', function () {
				$scope.$apply(function () { $scope.updateCurrentPanel(); });
			});

			//

			$scope.links = [];

			//

			page.constants.$promise.then(function () {
				page.tooltips.add({
					type: 'green',
					$target: $('#toolbar_contact'),
					message: page.constants.message('toolbar.contact')
				})
			});
		};

		return {
			restrict: 'A',
			templateUrl: 'toolbar.html',
			replace: true,
			controller: controller
		};
	}]);
});
