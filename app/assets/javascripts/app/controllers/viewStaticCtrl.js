define([
	'app/modules/dbControllers'
], function (db) {
	'use strict';

	db.controller('ViewStaticCtrl', ['$scope', '$routeParams', '$location', '$http', function ($scope, $routeParams, $location, $http) {
		$scope.content = {
			templatePrefix: '/about/',
			templateSuffix: '',
			templateUrl: undefined,
			routePage: undefined
		};

		$scope.sidebar = {
			links: [
//				{
//					text: undefined,
//					id: undefined,
//					url: undefined,
//					links: {}
//				}
			]
		};

		//

		$scope.loadPage = function () {
			$scope.content.routePage = $routeParams.page;
			$scope.content.templateUrl = $scope.content.templatePrefix + $scope.content.routePage.replace('/','_') + $scope.content.templateSuffix;

			$http.get($scope.content.templateUrl).then(function (data) {
				console.log(arguments);
			});
		};

		$scope.updatePage = function (page) {
			$location.path(page);

			$scope.loadPage();
		};

		//

		var initialize = function () {
			$scope.loadPage();

			console.log($scope, $routeParams, $location);

			// parse sidebar.tmpl into object
		};

		initialize();
	}]);
});
