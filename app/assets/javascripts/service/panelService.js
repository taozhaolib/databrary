module.factory('panelService', [
	'$rootScope',
	'$location',
	'eventService',
	'$timeout',
	'arrayHelper',
	'guiService',
	function ($rootScope, $location, events, $timeout, arrayHelper, gui) {
		var panels = arrayHelper([]);

		//

		var addFn = panels.add;

		panels.add = function (panel) {
			var newPanel = addFn(panel);

			if (angular.isFunction(newPanel.bootPanel)) {
				newPanel.bootPanel();
			}

			if (angular.isFunction(newPanel.refreshPanel)) {
				newPanel.refreshPanel();
			}

			return newPanel;
		};

		//

		panels.enable = function (panel) {
			return panels.toggle(panel, 'enabled', true);
		};

		panels.disable = function (panel) {
			return panels.toggle(panel, 'enabled', false);
		};

		//

		panels.toggleFold = function (panel, state) {
			if (!panel.foldable) {
				return undefined;
			}

			return panel.toggleFold(state);
		};

		//

		panels.focus = function (panel) {
			if (angular.isFunction(panel.toggleFold)) {
				panel.toggleFold(false);
			}

			var $window = $(window),
				$document = $(document),
				oldHeight = 0,
				newHeight = 0;

			var checkHeight = function () {
				newHeight = $document.innerHeight();

				if (oldHeight == newHeight) {
					gui.scrollTo(panel.id);
				} else {
					$timeout(function () {
						checkHeight();
					}, 150);
					oldHeight = newHeight;
				}
			};

			checkHeight();

			return panel;
		};

		//

		panels.refresh = function () {
			angular.forEach(panels, function (panel) {
				if (angular.isFunction(panel.refresh)) {
					panel.refreshPanel();
				}
			});
		};

		//

		events.listen($rootScope, 'panelService-refresh', function () {
			panels.refresh();
		});

		$rootScope.$on('$routeChangeSuccess', function () {
			panels.reset();
		});

		//

		return panels;
	}
]);
