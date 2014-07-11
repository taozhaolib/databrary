'use strict';

module.factory('resourceFactory', [
	'$rootScope', '$cacheFactory', '$resource', function ($rootScope, $cacheFactory, $resource) {
		return function (url, params, methods, cacheID) {
			cacheID = angular.isString(methods) ? methods : !cacheID ? false : cacheID === true ? '$http' : cacheID;

			var cache;

			if (cacheID) {
				cache = $cacheFactory(cacheID);

				methods = angular.isObject(methods) ? methods : {};
				methods.get = methods.get || {method: 'GET'};
				methods.query = methods.query || {method: 'GET', isArray: true};

				for (var prop in methods) {
					if (methods.hasOwnProperty(prop) && methods[prop].method.toUpperCase() === 'GET') {
						methods[prop].cache = cache;
					}
				}
			}

			var resource = $resource(url, params, methods);

			if (cache) {
				resource.$cache = cache;
			}

			return resource;
		};
	}
]);
