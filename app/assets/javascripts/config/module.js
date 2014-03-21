define([
	'angular',

	'ngRoute',
	'ngSanitize',
	'ngStorage',
	'ngResource',
	'bindonce'
], function (angular) {
	'use strict';

	var module = angular.module('databraryModule', [
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

	module.config(['$provide', function ($provide) {
		$provide.decorator('$templateCache', ['$delegate', '$http', '$injector', function ($delegate, $http, $injector) {

			var promise,
				returned = false,
				allTemplatesUrl = '/public/templates/_all.html';

			var loadAllTemplates = function (url) {
				if (!promise) {
					promise = $http.get(allTemplatesUrl).then(function (response) {
						$injector.get('$compile')(response.data);
						return response;
					});
				}

				return promise.then(function (response) {
					returned = true;

					return {
						status: response.status,
						data: get(url)
					};
				});
			};

			var get = $delegate.get;

			$delegate.get = function (url) {
				if (!returned) {
					return loadAllTemplates(url);
				}

				return get(url);
			};

			return $delegate;
		}]);
	}]);

	module.run(['$rootScope', '$location', 'RouterService', 'BrowserService', 'ConstantService', 'AuthService', 'TypeService', 'PlayService', function ($rootScope, $location, router, browser, constant, auth, type, playService) {
		// $rootScope specials -- TODO: cut out some of these
		$rootScope.router = router;
		$rootScope.browser = browser;
		$rootScope.constant = constant;
		$rootScope.auth = auth;
		$rootScope.type = type;

		playService.run();

		// TODO: anchor scroll on page load. I think the problem is animated items on load.
//		$location.hash('');
	}]);

	return module;
});
