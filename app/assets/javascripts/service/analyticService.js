module.factory('analyticService', [
	'$rootScope', '$location', function ($rootScope, $location) {
		var analytics = {};

		var queue = [];

		//

		analytics.dump = function () {
			if(!queue.length)
				return false;

			return JSON.stringify(queue.splice(0, queue.length));
		};

		analytics.add = function (action, route, data) {
			queue.push({
				action: action,
				route: route,
				data: data
			});
		};

		//

		//$rootScope.$on('$routeChangeStart', function (event, next, current) {});

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

		//$rootScope.$on('$routeUpdate', function (event, current) {});

		//

		return analytics;
	}
]);

