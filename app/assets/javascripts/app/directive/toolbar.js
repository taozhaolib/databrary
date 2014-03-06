define(['app/config/module'], function (module) {
	'use strict';

	module.directive('toolbar', ['$location', '$anchorScroll', '$timeout', 'EventService', 'AuthService', 'PanelService', function ($location, $anchorScroll, $timeout, eventService, authService, panelService) {
		var controller = function ($scope, $element, $attrs) {
			$scope.auth = $scope.auth || authService;

			//

			$scope.panels = [];

			$scope.updatePanels = function () {
				$scope.panels = panelService.all();
			};

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

			$scope.links = [
				{
					url: 'https://www.facebook.com/pages/Databrary/185349568273416',
					target: '_blank',
					classes: 'toolbar_img16',
					title: 'Facebook',
					image: '/public/images/social/16px/facebook.png'
				},
				{
					url: 'https://plus.google.com/u/1/111083162045777800330/posts',
					target: '_blank',
					classes: 'toolbar_img16',
					title: 'Google+',
					image: '/public/images/social/16px/google-plus.png'
				},
				{
					url: 'https://twitter.com/databrary',
					target: '_blank',
					classes: 'toolbar_img16',
					title: 'Twitter',
					image: '/public/images/social/16px/twitter.png'
				},
				{
					url: 'https://github.com/databrary/',
					target: '_blank',
					classes: 'toolbar_img16',
					title: 'GitHub',
					image: '/public/images/social/16px/github.png'
				}
			];

			//

			eventService.listen($scope, 'toolbarCtrl-updatePanels', function () {
				$scope.updatePanels();
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
