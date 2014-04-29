module.factory('analyticService', [
	'$rootScope', '$location', '$cacheFactory', '$injector', function ($rootScope, $location, $cacheFactory, $injector) {
		var analytics = {};

		var queue = [];

		//

		analytics.dump = function (config) {
			if (!queue.length)
				return false;

			var info = $cacheFactory.info();
			var cache, key;

			for (key in info) {
				if (config.url.indexOf(key) > -1) {
					cache = $cacheFactory.get(key);

					if (!cache.get(config.url))
						return JSON.stringify(queue.splice(0, queue.length));

					break;
				}
			}

			return false;
		};

		analytics.add = function (action, route, data) {
			var analytic = {};

			analytic.action = action;
			analytic.route = angular.isString(route) ? route : $location.url();
			analytic.data = angular.isObject(route) ? route : data || {};

			queue.push(analytic);
		};

		//

		$rootScope.$on('$routeChangeSuccess', function (event, current, previous) {
			analytics.add('open', $location.url(), {
				current: current.controller,
				previous: previous.controller
			});
		});

		$rootScope.$on('$routeChangeError', function (event, next, current, error) {
			analytics.add('close', $location.url(), {
				next: next.controller,
				current: current.controller,
				error: error
			});
		});

		//

		return analytics;
	}
]);

