define([
	'app/modules/dbDirectives',
	'app/services/panelService'
], function (db) {
	'use strict';

	db.directive('panel', ['PanelService', function (panelService) {
		var link = function ($scope, $element, $attrs) {
			var panelCtrl = panelService.getController();

			$scope.isEnabled = $attrs.dbPanelEnabled != "false";
			$element.removeAttr('db-panel-enabled');

			$scope.title = ($attrs.dbPanelTitle != "") ? $attrs.dbPanelTitle : $element.attr('id').split('_').pop();
			$element.removeAttr('db-panel-title');

			$scope.id = $element.attr('id');

			//

			$scope.panelEnable = function () {
				$scope.isEnabled = true;
			};

			$scope.panelDisable = function () {
				$scope.isEnabled = false;
			};

			$scope.panelToggle = function () {
				if ($scope.isEnabled)
					$scope.panelEnable();
				else
					$scope.panelDisable();
			};

			//

			panelCtrl.createPanel($scope);
		};

		return {
			restrict: 'A',
			scope: true,
			priority: 100,
			link: link
		};
	}]);
});
