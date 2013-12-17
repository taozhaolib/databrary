define(['app/config/module'], function (module) {
	'use strict';

	module.factory('resourceService', ['$rootScope', '$resource', '$cacheFactory', function ($rootScope, $resource, $cacheFactory) {
		return function (id, url, paramDefaults, hotParamsArray, actions) {
			var resource = {};

			resource.cache = $cacheFactory(id + 'Cache');
			resource.resource = $resource(url, paramDefaults, actions);

			// TODO: implement queue so that objects are updated before the next is get
			// var queue = [];

			//

			var updateCacheObject = function (key, data) { console.log(data);
				var obj = resource.cache.get(key);

				if (obj)
					return resource.cache.put(key, angular.extend(obj, data));
				else
					return resource.cache.put(key, data);
			};

			//

			resource.get = function (key, params) {
				params = angular.isObject(params) ? params : {};

				var result = resource.cache.get(key);

				if (result) {
					var incomplete = false;

					angular.forEach(params, function (value, key) {
						if (hotParamsArray.indexOf(key) > -1 && !result.hasOwnProperty(key))
							incomplete = true;
					});

					if (!incomplete)
						return result;
				}

				return resource.resource.get(params, function (data) {
					return updateCacheObject(data.id, data);
				});
			};

			//

			return resource;
		};
	}]);
});
