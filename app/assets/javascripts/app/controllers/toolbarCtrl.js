define([
	'app/modules/dbControllers',
	'app/services/eventService',
	'app/services/authService'
], function (db) {
	'use strict';

	db.controller('ToolbarCtrl', ['$scope', '$location', '$anchorScroll', '$timeout', 'EventService', 'AuthService', 'PanelService', function ($scope, $location, $anchorScroll, $timeout, eventService, authService, panelService) {
		$scope.panels = [];
		$scope.links = {
			left: [],
			right: []
		};

		$scope.authUser = authService.getAuthUser();

		$scope.scrollTo = function (panel) {
			$location.hash(panel.id);
			$anchorScroll();
		};

		//

		$scope.logIn = authService.logIn;
		$scope.logOut = authService.logOut;
		$scope.isUser = authService.isUser;

		$scope.enableSU = authService.enableSU;
		$scope.disableSU = authService.disableSU;
		$scope.toggleSU = authService.toggleSU;
		$scope.isSU = authService.isSU;

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

		$scope.updatePanels = function () {
			$scope.panels = panelService.getPanels();
		};

		//

		$scope.updateLinks = function (links) {
			if(angular.isObject(links)) {
				$scope.links.left = links.left;
				$scope.links.right = links.right;
			}

			if(angular.isArray(links)) {
				$scope.links.left = links;
			}

			return false;
		};

		//

		eventService.listen($scope, 'toolbarCtrl-updatePanels', function ($event, context) {
			$scope.updatePanels();

			if(context !== false)
				$scope.setContext('panels');
		});

		eventService.listen($scope, 'toolbarCtrl-updateLinks', function ($event, links, context) {
			$scope.updateLinks(links);

			if(context !== false)
				$scope.setContext('links');
		});

		eventService.listen($scope, 'toolbarCtrl-setContext', function ($event, check) {
			$scope.setContext(check);
		});
	}]);
});
