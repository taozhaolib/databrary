define([
	'app/modules/dbDirectives',
	'app/services/panelService'
], function (db) {
	'use strict';

	db.directive('panel', ['PanelService', function (panelService) {
		var link = function ($scope, $element, $attrs) {
			$scope.isEnabled = $attrs.dbPanelEnabled != "false";
			$element.removeAttr('db-panel-enabled');

			$scope.title = ($attrs.dbPanelTitle != "") ? $attrs.dbPanelTitle : $element.attr('id').split('_').pop();
			$element.removeAttr('db-panel-title');

			$scope.id = $element.attr('id');

			//

			$scope.enablePanel = function () {
				$scope.isEnabled = true;
			};

			$scope.disablePanel = function () {
				$scope.isEnabled = false;
			};

			$scope.togglePanel = function () {
				if ($scope.isEnabled)
					$scope.enablePanel();
				else
					$scope.disablePanel();
			};

			//

			$scope.foldPanel = function () {
				if (typeof($scope.foldUp) != 'undefined')
					$scope.foldUp();
			};

			$scope.unfoldPanel = function () {
				if (typeof($scope.foldDown) != 'undefined')
				$scope.foldDown();
			};

			//

			panelService.createPanel($scope);
		};

		return {
			restrict: 'A',
			scope: true,
			priority: 100,
			link: link
		};
	}]);
});
