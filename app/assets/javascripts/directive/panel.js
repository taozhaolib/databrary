define(['config/module'], function (module) {
	'use strict';

	module.directive('panel', ['panelService', function (panels) {
		var link = function ($scope, $element, $attrs, ctrl, transclude) {
			$scope.id = (angular.isDefined($attrs.id)) ? $attrs.id : '';
			$scope.title = $attrs.panelTitle || '';
			$scope.top = (angular.isDefined($attrs.top) && $attrs.top != 'false') ? true : false;
			$scope.enabled = true;

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
				classes[$scope.id] = true;

				return classes;
			};

			//

			transclude($scope, function ($clone) {
				$element.find('[panel-body]').append($clone);
			});

			panels.add($scope);
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
