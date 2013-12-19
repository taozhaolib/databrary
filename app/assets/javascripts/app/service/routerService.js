define(['app/config/module'], function (module) {
	'use strict';

	module.factory('RouterService', ['$rootScope', '$route', '$filter', function ($rootScope, $route, $filter) {
		var routerService = {};

		routerService.$route = $route;

		//

		routerService.makeUrl = function (url, params) {
			if (!params) return url;

			var parts = [];

			angular.forEach(params, function (value, key) {
				if (value === null || angular.isUndefined(value))
					return;

				if (!angular.isArray(value))
					value = [value];

				angular.forEach(value, function (v) {
					if (angular.isObject(v))
						v = angular.toJson(v);

					var regex = new RegExp(':' + key + '\\*?'),
						match = url.match(regex);

					if (match.length > 0)
						url = url.replace(regex, v);
					else
						parts.push($filter('uri')(key, true) + '=' +
							$filter('uri')(v, true));
				});
			});

			if (parts.length > 0)
				url += ((url.indexOf('?') == -1) ? '?' : '&');

			return url + parts.join('&');
		};

		//

		var createRoutes = function () {
			angular.forEach($route.routes, function (config, route) {
				if (route == 'null' || route.length == 0 || (route.length > 1 && route.substr(route.length - 1) == '/'))
					return;

				var key = route
					.split('/')
					.filter(function (e) {
						return e.length > 0 && e.substr(0, 1) != ':';
					})
					.map(function (e, i) {
						return i == 0 ? e : e.charAt(0).toUpperCase() + e.slice(1);
					})
					.join('');

				if (!key)
					key = 'index';

				routerService[key] = function (params) {
					return routerService.makeUrl(route, params);
				}
			});
		};

		createRoutes();

		//

		return routerService;
	}]);
});
