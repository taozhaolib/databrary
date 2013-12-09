define([
	'app/modules/dbControllers'
], function (db) {
	'use strict';

	db.controller('ViewStaticCtrl', ['$scope', '$routeParams', function ($scope, $routeParams) {
		$scope.apiPrefix = '/api/about/';
		$scope.apiSuffix = '';

		$scope.urlPrefix = '/about/';
		$scope.urlSuffix = '';

		$scope.content = {
			templateUrl: undefined
		};

		$scope.sidebar = {
			enabled: true,
			links: []
		};

		//

		$scope.getUrl = function (page) {
			if (angular.isUndefined(page))
				return $scope.urlPrefix.slice(0, -1);

			return $scope.urlPrefix + page + $scope.urlSuffix;
		};

		$scope.getApi = function (page) {
			if (angular.isUndefined(page))
				return $scope.apiPrefix.slice(0, -1);

			return $scope.apiPrefix + page + $scope.apiSuffix;
		};

		//

		$scope.isCurrent = function (page) {
			return $scope.content.templateUrl == $scope.getApi(page);
		};

		$scope.getClasses = function (link) {
			return {
				'current': $scope.isCurrent(link.path)
			};
		};

		//

		$scope.loadPage = function () {
			$scope.content.templateUrl = $scope.getApi($routeParams.page);
		};

		//

		$scope.initializeSidebar = function (links) {
			if (!angular.isArray(links)) {
				$scope.sidebar.enabled = false;
				return;
			}

			var l = links.length,
				hold;

			for (var i = 0; i < l; i++) {
				links[i].url = $scope.getUrl(links[i].path);

				if (links[i].parent == "true") {
					if (angular.isDefined(hold)) {
						$scope.sidebar.links.push(hold);
					}

					links[i].links = [];
					hold = links[i];
				} else {
					if (angular.isDefined(hold))
						hold.links.push(links[i]);
					else
						$scope.sidebar.links.push(links[i]);
				}
			}

			if (angular.isDefined(hold))
				$scope.sidebar.links.push(hold);
		};

		$scope.enableSidebar = function () {
			$scope.sidebar.enabled = true;
		};

		$scope.disableSidebar = function () {
			$scope.sidebar.enabled = false;
		};

		//

		var initialize = function () {
			$scope.loadPage();
		};

		initialize();
	}]);
});
