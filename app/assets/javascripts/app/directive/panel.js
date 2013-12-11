define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panel', ['PanelService', function (panelService) {
		var link = function ($scope, $element, $attrs) {
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
				if (typeof($scope.fold) != 'undefined')
					$scope.fold();
			};

			$scope.unfoldPanel = function () {
				if (typeof($scope.unfold) != 'undefined')
				$scope.unfold();
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

			$scope.getPanelClasses = function () {
				var classes = {};

				classes['panel'] = true;
				classes[$scope.panel.id] = true;

				return classes;
			};

			//

			var start = function () {
				$scope.panel = {
					id: (angular.isDefined($attrs.id)) ? $attrs.id : '',
					title: (angular.isDefined($attrs.title)) ? $attrs.title : '',
					top: (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false
				};

				panelService.createPanel($scope);
			};

			start();
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'panel.html',
			transclude: true,
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
