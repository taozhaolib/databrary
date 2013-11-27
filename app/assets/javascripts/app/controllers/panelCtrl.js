define([
	'app/modules/dbControllers',
	'app/services/eventService',
	'app/services/panelService'
], function (db) {
	'use strict';

	db.controller('PanelCtrl', ['$scope', '$sessionStorage', 'PanelService', 'EventService', function ($scope, $sessionStorage, panelService, eventService) {
		$scope.$storage = $sessionStorage;

		panelService.setController($scope);

		//

		$scope.panels = {};

		$scope.getPanelId = function (panel) {
			var id = (typeof(panel) == 'object') ? panel.$id : panel;

			if ($scope.panels[id])
				return id;

			return false;
		};

		$scope.getPanel = function (panel) {
			var id = (typeof(panel) == 'object') ? panel.$id : panel;

			if ($scope.panels[id])
				return $scope.panels[id];

			return false;
		};

		eventService.broadcast('toolbarsPanels', $scope.panels);

		//

		$scope.addPanel = function (panel) {
			$scope.panels[panel.$id] = panel;
		};

		$scope.createPanel = function (panel) {
			var id = $scope.getPanelId(panel);

			if (id)
				return $scope.updatePanel(panel);

			return $scope.addPanel(panel);
		};

		$scope.updatePanel = function (panel) {
			var id = $scope.getPanelId(panel);

			if (id)
				return $scope.panels[id] = $.extend(true, {}, $scope.panels[id], panel);

			return false;
		};

		$scope.deletePanel = function (panel) {
			var id = $scope.getPanelId(panel),
				old = $scope.panels[id];

			if (old && delete $scope.panels[id])
				return old;

			return false;
		};

		//

		$scope.$watch(function () {
			var list = '';

			for (var id in $scope.panels)
				list += $scope.panels[id].isFolded;

			return list;
		}, function (newValue, oldValue, scope) {

		}, true);
	}]);
});
