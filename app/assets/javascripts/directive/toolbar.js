module.directive('toolbar', [
	'pageService', function (page) {
		var controller = function ($scope) {
			$scope.auth = $scope.auth || page.auth;

			//

			$scope.panels = page.panels;

			$scope.focusPanel = function (panel) {
				page.panels.focus(panel);
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
				for (var i = page.panels.length - 1; i >= 0; i--) {
					if (page.panels[i].isCurrent()) {
						currentPanel = page.panels[i];
						break;
					}
				}
			};

			//

			$scope.betaHome = function () {
				if (page.auth.hasAuth('VIEW')) {
					return '/home';
				}

				return '/';
			};

			//

			$w.on('scroll resize', function () {
				$scope.$apply(function () {
					$scope.updateCurrentPanel();
				});
			});

			//

			$scope.links = [];

			//

			page.constants.$promise.then(function () {
				page.tooltips.add({
					type: 'green',
					$target: $('#toolbar_contact'),
					message: page.constants.message('toolbar.contact')
				})
			});
		};

		return {
			restrict: 'A',
			templateUrl: 'toolbar.html',
			replace: true,
			controller: controller
		};
	}
]);
