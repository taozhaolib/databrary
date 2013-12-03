define([
	'angular',
	'app/modules/dbDirectives',
	'app/services/eventService',
	'app/services/panelService'
], function (angular, db) {
	'use strict';

	db.directive('contextbar', ['$location', 'EventService', 'PanelService', function ($location, eventService, panelService) {
		var compile = function ($element, $attrs) {

		};

		var link = function ($scope, $element, $attrs) {
			$scope.contextbar = {
				mode: undefined,
				panels: undefined,
				linksLeft: [],
				linksRight: []
			};

			//

			$scope.updateContext = function (panels, links) {

				if (panels)
					$scope.updateContextPanels(panels);
				else
					$scope.updateContextLinks(links);
			};

			$scope.updateContextPanels = function (panels) {
				if (angular.isDefined(panels)) {
					$scope.contextbar.panels = panels;
				}

				if ($location.absUrl().indexOf('/volume/') > -1)
					$scope.contextbar.mode = 'panels';
			};

			$scope.updateContextLinks = function (links) {
				if (angular.isDefined(links)) {
					$scope.contextbar.linksLeft = (links.hasOwnProperty('left')) ? links.left : links;
					$scope.contextbar.linksRight = (links.hasOwnProperty('right')) ? links.right : [];
				}

				$scope.contextbar.mode = 'links';
			};

			//

			eventService.listen($scope, 'contextbarUpdate', function ($event, panels, links) {
				// TODO: use $route and $scope.$on('$routeChangeSuccess', function ($event, $current, $previous) {}); when routes are implemented

				$scope.updateContext(panels, links);
			});

			//

			$scope.updateContextLinks($scope.tempContextLinks);
			delete $scope.tempContextLinks;
		};

		return {
			restrict: 'A',
			link: link
		}
	}]);
});
