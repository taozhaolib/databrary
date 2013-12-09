define([
	'app/modules/dbControllers'
], function (db) {
	'use strict';

	db.controller('ViewStaticCtrl', ['$scope', '$routeParams', 'EventService', function ($scope, $routeParams, eventService) {
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

		var start = function () {
			$scope.loadPage();

			var tempLinks = {
				left: $scope.sidebar.links,
				right: [
					{
						url: 'https://www.facebook.com/pages/Databrary/185349568273416',
						target: '_blank',
						classes: 'toolbar_img16',
						title: 'Facebook',
						image: '/public/images/social/16px/facebook.png'
					},
					{
						url: 'https://plus.google.com/u/1/111083162045777800330/posts',
						target: '_blank',
						classes: 'toolbar_img16',
						title: 'Google+',
						image: '/public/images/social/16px/google-plus.png'
					},
					{
						url: 'https://twitter.com/databrary',
						target: '_blank',
						classes: 'toolbar_img16',
						title: 'Twitter',
						image: '/public/images/social/16px/twitter.png'
					},
					{
						url: 'https://github.com/databrary/',
						target: '_blank',
						classes: 'toolbar_img16',
						title: 'GitHub',
						image: '/public/images/social/16px/github.png'
					}
				]};

			// TODO: send real links
			eventService.talk('toolbarCtrl-updateLinks', tempLinks, true);
		};

		start();
	}]);
});
