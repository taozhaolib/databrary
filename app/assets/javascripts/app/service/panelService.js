define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PanelService', ['$rootScope', '$location', '$anchorScroll', 'EventService', '$timeout', function ($rootScope, $location, $anchorScroll, eventService, $timeout) {
		var panelService = {};

		var panels = undefined;

		//

		var getPanelById = function (panel) {
			var index;

			panels.some(function (el, i) {
				if (el.$id == panel) {
					index = i;
					return true;
				}

				return false;
			});

			if (!angular.isNumber(index))
				return undefined;

			return panels[index];
		};

		var getPanelByObj = function (panel) {
			var index = panels.indexOf(panel);

			if (!~index)
				return undefined;

			return panels[index];
		};

		//

		panelService.getPanel = function (panel) {
			if (angular.isObject(panel))
				return getPanelByObj(panel);
			else
				return getPanelById(panel);
		};

		panelService.getPanelIndex = function (panel) {
			panel = panelService.getPanel(panel);

			var index = panels.indexOf(panel);

			if (!~index)
				return undefined;

			return index;
		};

		//

		panelService.createPanel = function (panel) {
			var index = panelService.getPanelIndex(panel);

			if (angular.isNumber(index))
				return panelService.updatePanel(panels[index], panel);

			panels.push(panel);

			return panels[panels.length - 1];
		};

		panelService.updatePanel = function (old, panel) {
			var index = panelService.getPanelIndex(old);

			if (!angular.isNumber(index))
				return panelService.createPanel(panel);

			angular.extend(panels[index], panel);

			return panels[index];
		};

		panelService.deletePanel = function (panel) {
			var index = panelService.getPanelIndex(panel);

			if (!angular.isNumber(index))
				return undefined;

			return panels.splice(index, 1).pop();
		};

		//

		panelService.getPanels = function () {
			return panels;
		};

		panelService.resetPanels = function () {
			panels = [];

			return panels;
		};

		//

		panelService.enablePanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.enablePanel();

			return panel;
		};

		panelService.disablePanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.disablePanel();

			return panel;
		};

		panelService.togglePanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.togglePanel();

			return panel;
		};

		//

		panelService.foldPanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.foldPanel();

			return panel;
		};

		panelService.unfoldPanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.unfoldPanel();

			return panel;
		};

		//

		panelService.focusPanel = function (panel) {
			panel = panelService.getPanel(panel);

			if (!angular.isObject(panel))
				return undefined;

			panel.unfoldPanel();

			var $document = $(document),
				oldHeight = 0,
				newHeight = 0,
				rate = 100,
				timeout;

			var checkHeight = function () {
				newHeight = $document.innerHeight();

				if(oldHeight == newHeight) {
					$location.hash(panel.id);
					$anchorScroll();
				}else{
					timeout = $timeout(function () {
						checkHeight();
					}, rate++);
					oldHeight = newHeight;
				}
			};

			checkHeight();

			return panel;
		};

		//

		$rootScope.$on('$routeChangeSuccess', function () {
			panelService.resetPanels();
		});

		$rootScope.$watch(function () {
			return panels.join('-');
		}, function () {
			if (panels.length > 0)
				eventService.talk('toolbarCtrl-updatePanels', true);
		});

		//

		var start = function () {
			panelService.resetPanels();
		};

		start();

		//

		return panelService;
	}]);
});
