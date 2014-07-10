'use strict';

module.factory('analyticInterceptor', [
	'$rootScope', 'analyticService', function ($rootScope, analytics) {
		return {
			request: function (config) {
				var json = analytics.dump(config);

				if (json) {
					config.headers.Analytics = json;
				}

				return config;
			}
		};
	}
]);

module.config([
	'$httpProvider', function ($httpProvider) {
		$httpProvider.interceptors.push('analyticInterceptor');
	}
]);
