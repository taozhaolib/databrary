define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panel', ['PanelService', 'ArrayHelper', function (panelService, arrayHelper) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
				$scope.modes = arrayHelper([]);
				$scope.enabled = true;

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

				$scope.activateMode = function (mode) {
					for (var i = 0; i < $scope.modes.length; i++) {
						$scope.modes[i].active = $scope.modes[i] == mode;
					}
				};

				$scope.updateModes = function () {
					if($scope.modes.length == 1 && !$scope.modes[0].active)
						$scope.modes[0].active = true;
				};

				$scope.showModeLinks = function () {
					return $scope.modes.length > 1;
				};

				$scope.getModeLinkClasses = function (mode) {
					var classes = {};

					classes['panel_mode_link'] = true;
					classes['panel_mode_'+mode.name] = true;
					classes['active'] = mode.active;

					return classes;
				};

				//

				var start = function () {
					$scope.panel = $scope;
					$scope.id = (angular.isDefined($attrs.id)) ? $attrs.id : '';
					$scope.title = (angular.isDefined($attrs.title)) ? $attrs.title : '';
					$scope.top = (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false;

					$element.attr('title', '');

					$scope.container = $scope;

					transclude($scope, function ($clone) {
						$element.find('[panel-body]').append($clone);
					});

					if(angular.isFunction($scope.bootPanel))
						$scope.bootPanel();

					panelService.add($scope);
				};

				start();
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'panel.html',
			transclude: true,
			replace: true,
			priority: 100,
			compile: compile
		};
	}]);
});
