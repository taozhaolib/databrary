module.factory('analyticInterceptor', ['$rootScope', 'analyticService', function ($rootScope, analytics) {
	return {
		request: function (config) {
			var queue = analytics.next();

			if (queue)
				config.headers['Analytics'] = queue;

			return config;
		},

		requestError: function (rej) {
			console.log(rej);
		}
	}
}]);
