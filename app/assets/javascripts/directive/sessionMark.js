define(['config/module'], function (module) {
	'use strict';

	module.directive('sessionMark', ['MessageService', 'ConstantService', function (messages, constant) {
		var link = function ($scope, $el, $attrs) {
			var body;

			$scope.mark = $attrs.mark;

			switch ($scope.mark) {
				case 'excluded':
					if(!$scope.data.object.categories[-700])
						return $el.remove();

					body = constant.message('notice.help.exclusion', $scope.data.volume.records[$scope.data.object.categories[-700][0].id].measures.reason);
					break;

				case 'pilot':
					if(!$scope.data.object.categories[-800])
						return $el.remove();

					body = constant.message('notice.help.pilot');
					break;

				default:
					return $el.remove();
					break;
			}

			var message = messages.add({
				type: 'trace',
				enabled: false,
				body: body
			});

			//

			$scope.markEnter = function () {
				messages.enable(message);
			};

			$scope.markLeave = function () {
				messages.disable(message);
			};

			$scope.$on('$destroy', function () {
				messages.remove(message);
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
			scope: {
				'data': '='
			},
			replace: true,
			link: link
		};
	}]);
});
