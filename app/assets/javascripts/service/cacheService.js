module.factory('cacheService', [
	'$rootScope', function ($rootScope) {
		var hash = {};
		var routes = {};

		var globalController = {
			reload: function () {
				angular.forEach(hash, function (thisCache, id) {
					thisCache.cache = {};
				});
			},

			halt: function () {
				angular.forEach(hash, function (thisCache, id) {
					thisCache.halt = true;
				});
			},

			resume: function () {
				angular.forEach(hash, function (thisCache, id) {
					thisCache.halt = false;
				});
			}
		};

		var parseParams = function (params) {
			if (angular.isObject(params)) {
				var temp = [];

				angular.forEach(params, function (val, key) {
					temp.push(key);
				});

				return temp;
			} else if (!angular.isArray(params)) {
				return [];
			}

			return params;
		};

		var cacheFactory = function (cacheID) {
			if (hash[cacheID]) {
				return hash[cacheID].controller;
			} else if (typeof cacheID === 'undefined') {
				return globalController;
			}

			var thisCache = hash[cacheID] = {
				halt: false,
				cache: {},
				unmarked: [],

				controller: {
					get: function (id, config) {
						if (thisCache.halt) {
							return false;
						} else if (routes[config.url] && routes[config.url].length > 1) {
							return routes[config.url];
						} else if (!thisCache.cache[id]) {
							return false;
						}

						var params = parseParams(config.params);

						for (var i = 0, l = params.length; i < l; i++) {
							if (thisCache.cache[id].params.indexOf(params[i]) == -1) {
								return false;
							}
						}

						return thisCache.cache[id].object;
					},

					set: function (object, config) {
						if (thisCache.halt || $.isEmptyObject(object) || !object.id) {
							return false;
						}

						var params = parseParams(config.params);

						if (thisCache.cache[object.id]) {
							angular.forEach(object, function (val, key) {
								thisCache.cache[object.id].object[key] = val;

								if (!thisCache.cache[object.id].params.indexOf(key) && !thisCache.unmarked.indexOf(key)) {
									thisCache.unmarked.push(key);
								}
							});

							angular.forEach(thisCache.unmarked, function (param) {
								if (!object[param]) {
									delete thisCache.cache[object.id].object[param];
								}
							});

							angular.forEach(params, function (param) {
								if (thisCache.cache[object.id].params.indexOf(param) == -1) {
									thisCache.cache[object.id].params.push(param);
								}

								if (!object[param]) {
									delete thisCache.cache[object.id].object[param];
								}
							});
						} else {
							angular.forEach(object, function (val, key) {
								if (params.indexOf(key) == -1) {
									thisCache.unmarked.push(key);
								}
							});

							thisCache.cache[object.id] = {
								params: params,
								object: object
							};
						}

						if (!routes[config.url])
							routes[config.url] = [];

						if (routes[config.url].indexOf(thisCache.cache[object.id].object) == -1) {
							routes[config.url].push(thisCache.cache[object.id].object);
						}

						return true;
					},

					remove: function (id) {
						delete thisCache.cache[id];
					},

					reload: function () {
						thisCache.cache = {};
					},

					halt: function () {
						thisCache.halt = true;
					},

					resume: function () {
						thisCache.halt = false;
					}
				}
			};

			return thisCache.controller;
		};

		return cacheFactory;
	}
]);
