define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PanelService', ['$rootScope', '$location', '$anchorScroll', 'EventService', '$timeout', 'ArrayHelper', function ($rootScope, $location, $anchorScroll, eventService, $timeout, arrayHelper) {
		var panelService = arrayHelper([]);

		//

		panelService.toggleEnabled = function (item, state) {
			return panelService.toggle(item, 'enabled', state);
		};

		//

		panelService.toggleFold = function (item, state) {
			return panelService.echo(item, function (item) {
				if (!item.isFoldable())
					return undefined;

				return item.toggleFold(state);
			});
		};

		//

		panelService.focus = function (item) {
			return panelService.echo(item, function (item) {
				item.toggleFold(false);

				var $document = $(document),
					oldHeight = 0,
					newHeight = 0,
					rate = 100,
					timeout;

				var checkHeight = function () {
					newHeight = $document.innerHeight();

					if (oldHeight == newHeight) {
						$location.hash(item.id);
						$anchorScroll();
					} else {
						timeout = $timeout(function () {
							checkHeight();
						}, rate++);
						oldHeight = newHeight;
					}
				};

				checkHeight();

				return item;
			});
		};

		//

		$rootScope.$on('$routeChangeSuccess', function () {
			panelService.reset();
		});

		$rootScope.$watch(function () {
			return panelService.join('-');
		}, function () {
			if (panelService.length > 0)
				eventService.talk('toolbarCtrl-updatePanels', true);
		});

		//

		return panelService;
	}]);
});
