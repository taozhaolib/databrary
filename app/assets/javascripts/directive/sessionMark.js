define(['config/module'], function (module) {
	'use strict';

	module.directive('sessionMark', ['ConstantService', 'TooltipService', function (constant, tooltips) {
		var link = function ($scope, $el, $attrs) {
			$scope.mark = $attrs.mark;
			var message = '';

			switch ($scope.mark) {
				case 'excluded':
					if(!$scope.data.object.categories[-700])
						return $el.remove();

					message = constant.message('notice.help.exclusion', $scope.data.volume.records[$scope.data.object.categories[-700][0].id].measures.reason);
					break;

				case 'pilot':
					if(!$scope.data.object.categories[-800])
						return $el.remove();

					message = constant.message('notice.help.pilot');
					break;

				default:
					return $el.remove();
					break;
			}

			var tooltip = tooltips.add({
				message: message,
				type: 'orange',
				$target: $el
			});

			//

			$scope.$on('$destroy', function () {
				tooltips.remove(tooltip);
			});

			//

			$scope.getMarkClasses = function () {
				var classes = [];

				classes.push('session_mark_'+$scope.mark);

				return classes;
			};
		};

		return {
			restrict: 'E',
			templateUrl: 'sessionMark.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
