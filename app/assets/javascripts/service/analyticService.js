module.factory('analyticService', ['$rootScope', function ($rootScope) {
	var analytics = {};

	var queue = [];
	var packages = [];

	//

	analytics.next = function () {
		packages.push(queue.splice(0, queue.length));
		var id = packages.length - 1;

		return {
			get: function () {
				return JSON.stringify(packages[id]);
			},
			undo: function () {
				queue.concat(packages[id]);
				return packages[id] = null;
			},
			clear: function () {
				return packages[id] = null;
			}
		};
	};

	//

//		$rootScope.$on('$routeChangeStart', function () {
//		});

//		$rootScope.$on('$routeChangeSuccess', function () {
//		});

//		$rootScope.$on('$routeChangeError', function () {
//		});

//		$rootScope.$on('$routeUpdate', function () {
//		});

	//

	return analytics;
}]);
