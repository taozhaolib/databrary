define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', function (browserService) {
		var link = function ($scope, $element, $attrs) {
			$scope.browserDepth = parseInt($attrs.level);

			$scope.getDepth = function () {
				return $scope.browserDepth;
			};

			//

			$scope.hasLevelItems = function (item) {
				return $scope.browser.hasLevelItems($scope.getDepth(), $scope.parentLevelArgs());
			};

			$scope.getLevelItems = function (item) {
				return $scope.browser.getLevelItems($scope.getDepth(), $scope.parentLevelArgs());
			};

			$scope.getLevelType = function () {
				return $scope.browser.getLevelType($scope.getDepth());
			};

			$scope.getLevelInclude = function () {
				var level = $scope.getLevelType(),
					tpl;

				if (['volume', 'session', 'asset'].indexOf(level) > -1)
					tpl = level;
				else
					tpl = 'record';

				return 'browser' + tpl.charAt(0).toUpperCase() + tpl.slice(1) + '.html';
			};

			//

			$scope.levelArgs = function (item) {
				if(angular.isUndefined(item))
					return {};

				var type = $scope.getLevelType();

				var args = {};

				args['parent'] = type;
				args[type] = item.id;

				return angular.extend({}, $scope.parentLevelArgs(), args);
			};

			$scope.parentLevelArgs = function () {
				var parent = $element.parent().scope();

				if(angular.isFunction(parent.levelArgs))
					return parent.levelArgs(parent.item);

				return {};
			};
		};

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
