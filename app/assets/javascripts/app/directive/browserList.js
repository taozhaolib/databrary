define(['app/config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', 'ConstantService', 'RouterService', function (browserService, $filter, constantService, router) {
		var link = function ($scope, $element, $attrs) {
			if(!$scope.browser)
				$scope.browser = browserService;

			if(!$scope.constant)
				$scope.constant = constantService;

			$scope.getInclude = function () {
				if ($scope.data.items[0])
					return 'browser' +
						$scope.data.items[0].type.charAt(0).toUpperCase() +
						$scope.data.items[0].type.slice(1) +
						'.html';
			};

			$scope.toggleExpand = function () {
				$scope.data = $scope.browser.setItemExpand($scope.data);
			};

			$scope.expanderClasses = function (data) {
				var classes = [];

				classes.push(data.expand ? 'active' : '');

				return classes;
			};

			$scope.setItemSelect = function (data) {
				$scope.browser.setItemSelect(data);
			};

			//

			$scope.getName = function (data) {
				switch($scope.browser.getItemType(data.object)) {
					case 'volume':
						return data.object.name;

					case 'record':
						var category = $scope.constant.data.category[data.object.category].name;
						return category.charAt(0).toUpperCase() + category.slice(1) + ': ' + (data.object.measures.ident || data.object.id);

					case 'session':
						return 'Session: ' + (data.object.name || data.object.id);
				}
			};

			//

			$scope.editLink = function (data) {
				switch($scope.browser.getItemType(data.object)) {
					case 'volume':
						return router.volumeEdit(data.object);

					case 'record':
						return router.recordEdit(data.object);

					case 'session':
						return router.slotEdit(data.object);
				}
			};

			$scope.canEdit = function (data) {

			}
		};

		return {
			restrict: 'E',
			scope: {
				'data': '='
			},
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
