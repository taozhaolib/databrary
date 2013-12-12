define(['app/config/module'], function (module) {
	'use strict';

	module.directive('panel', ['PanelService', function (panelService) {
		var compile = function ($element, $attrs, transclude) {
			return function ($scope, $element, $attrs) {
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

				$scope.setMode = function (mode) {
					if ($scope.getModeIndex(mode) == -1)
						$scope.panel.modes.push(mode);

					return mode;
				};

				$scope.getModeIndex = function (mode) {
					return $scope.panel.modes.indexOf(mode);
				};

				$scope.getMode = function (mode) {
					var index = $scope.getModeIndex(mode);

					return (index > -1) ? $scope.panel.modes[index] : undefined;
				};

				$scope.activateMode = function (mode) {
					for (var i = 0; i < $scope.panel.modes.length; i++) {
						$scope.panel.modes[i].active = $scope.panel.modes[i] == mode;
					}
				};

				$scope.updateModes = function () {
					if($scope.panel.modes.length == 1 && !$scope.panel.modes[0].active)
						$scope.panel.modes[0].active = true;
				};

				$scope.getModes = function () {
					return $scope.panel.modes;
				};

				$scope.showModeLinks = function () {
					return $scope.panel.modes.length > 1;
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
					$scope.panel = {
						modes: [],
						id: (angular.isDefined($attrs.id)) ? $attrs.id : '',
						title: (angular.isDefined($attrs.title)) ? $attrs.title : '',
						top: (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false
					};

					transclude($scope, function ($clone) {
						$element.find('[panel-body]').append($clone);
					});

					panelService.createPanel($scope);
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
