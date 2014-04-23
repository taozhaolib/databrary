module.factory('analyticInterceptor', [
	'$rootScope', 'analyticService', function ($rootScope, analytics) {
		return {
			request: function (config) {
				if (config.cache) {
					return config;
				}

				var json = analytics.dump();

				if (json) {
					config.headers['Analytics'] = json;
				}

				return config;
			}
		}
	}
]);

module.config([
	'$httpProvider', function ($httpProvider) {
		$httpProvider.interceptors.push('analyticInterceptor');
	}
]);
