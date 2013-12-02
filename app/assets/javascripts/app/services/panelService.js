define([
	'app/modules/dbServices'
], function (db) {
	'use strict';

	db.factory('PanelService', ['$rootScope', function ($rootScope) {
		var panelService = {},
			panelCtrl;

		//

		panelService.setController = function (controller) {
			panelCtrl = controller;
		};

		panelService.getController = function () {
			return panelCtrl;
		};

		//

		panelService.getPanels = function () {
			if (!panelCtrl)
				return false;

			return panelCtrl.getPanels();
		};

		//

		return panelService;
	}]);
});
