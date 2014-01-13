define(['app/config/module'], function (module) {
	'use strict';

	module.controller('ToolbarCtrl', ['$scope', '$location', '$anchorScroll', '$timeout', 'EventService', 'AuthService', 'PanelService', function ($scope, $location, $anchorScroll, $timeout, eventService, authService, panelService) {
		$scope.authService = authService;

		//

		var context = 'panels';

		$scope.getContext = function () {
			return context;
		};

		$scope.setContext = function (check) {
			context = check;
		};

		$scope.checkContext = function (check) {
			return context == check;
		};

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
			var spy = null;

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

		$scope.links = {
			left: [],
			right: [
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
			]
		};

		$scope.updateLinks = function (links) {
			if (angular.isObject(links)) {
				$scope.links.left = links.left;
				$scope.links.right = links.right;
			}

			if (angular.isArray(links)) {
				$scope.links.left = links;
			}

			return false;
		};

		$scope.getLinkClasses = function (link) {
			return {
				'current': $location.path().indexOf(link.url) > -1
			};
		};

		//

		eventService.listen($scope, 'toolbarCtrl-updatePanels', function ($event, context) {
			$scope.updatePanels();

			if (context !== false)
				$scope.setContext('panels');
		});

		eventService.listen($scope, 'toolbarCtrl-updateLinks', function ($event, links, context) {
			$scope.updateLinks(links);

			if (context !== false)
				$scope.setContext('links');
		});

		eventService.listen($scope, 'toolbarCtrl-setContext', function ($event, check) {
			$scope.setContext(check);
		});
	}]);
});
