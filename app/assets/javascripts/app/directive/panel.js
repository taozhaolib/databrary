define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panel', ['PanelService', function (panelService) {
		var link = function ($scope, $element, $attrs) {
			$scope.isEnabled = $attrs.dbPanelEnabled != "false";
			$element.removeAttr('db-panel-enabled');

			$scope.title = ($attrs.dbPanelTitle != "") ? $attrs.dbPanelTitle : $element.attr('id').split('_').pop();
			$element.removeAttr('db-panel-title');

			$scope.id = $element.attr('id');
			$scope.element = $element;

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

			$scope.isCurrent = function () {
				var $w = $(window),
					$m = $('#main');

				var eTop = $element.offset().top,
					eBottom = eTop + $element.outerHeight(),
					pTop = $w.scrollTop() + parseFloat($m.css('margin-top'));

				return eTop - pTop <= 0 && eBottom - pTop >= 0;
			};

			$scope.getClasses = function () {
				return {
					enabled: $scope.isEnabled(),
					folded: $scope.isFolded()
				};
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
