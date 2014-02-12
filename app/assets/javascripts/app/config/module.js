define([
	'angular',

	'ngAnimate',
	'ngCookies',
	'ngRoute',
	'ngSanitize',
	'ngStorage',

	'ngResource',
	'bindonce'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
		'ngAnimate',
		'ngCookies',
		'ngRoute',
		'ngSanitize',
		'ngStorage',
		'ngResource',
		'pasvaz.bindonce'
	]);

	module.config(['$httpProvider', '$logProvider', function ($httpProvider, $logProvider) {
		$httpProvider.defaults.cache = true;
		$httpProvider.defaults.headers.common['X-Requested-With'] = 'DatabraryClient';

		$logProvider.debugEnabled(true);
	}]);

	module.config(["$provide", function ($provide) {
		$provide.decorator("$templateCache", ["$delegate", "$http", "$injector", function ($delegate, $http, $injector) {

			var allTemplatesPromise,
				allTemplatesUrl = '/public/templates/_all.html';

			var loadAllTemplates = function (url) {
				if (!allTemplatesPromise) {
					allTemplatesPromise = $http.get(allTemplatesUrl).then(function (response) {
						$injector.get("$compile")(response.data);
						return response;
					});
				}

				return allTemplatesPromise.then(function (response) {
					return {
						status: response.status,
						data: get(url)
					};
				});
			};

			var get = $delegate.get;

			$delegate.get = function (url) {
				if (!allTemplatesPromise) {
					return loadAllTemplates(url);
				}

				return get(url);
			};

			return $delegate;
		}]);
	}]);

	module.run(['$window', '$rootScope', '$location', '$log', 'RouterService', 'BrowserService', 'ConstantService', 'AuthService', function ($window, $rootScope, $location, $log, router, browser, constant, auth) {
		// $rootScope specials
		$rootScope.$log = $log;
		$rootScope.router = router;
		$rootScope.browser = browser;
		$rootScope.constant = constant;
		$rootScope.auth = auth;

		// play->angular redirects
		if (angular.isDefined($window.$play) && $window.$play.redirect) {
			switch (browser.getItemType($window.$play.object)) {
				case 'session':
					$location.url('/volume/' + $window.$play.object.volume + '?session_limit=' + $window.$play.object.id);
					break;

				case 'record':
					constant.$promise.then(function (data) {
						$location.url('/volume/' + $window.$play.object.volume + '?' + constant.data.category[$window.$play.object.category].name + '_limit=' + $window.$play.object.id);
					});
					break;

				case 'asset':
					// asset
					$location.url('/volume/' + $window.$play.object.container.volume + '?session_limit=' + $window.$play.object.container.id + '&asset_limit=' + $window.$play.object.asset.id);
					break;
			}
		}

		// TODO: anchor scroll on page load. I think the problem is animated items on load.
		$location.hash('');
	}]);

	return module;
});
