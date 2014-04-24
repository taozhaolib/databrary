module.factory('cacheService', [
	'$rootScope', function ($rootScope) {
		var hash = {};
		var arrays = {};

		var props = {
			'records': 'record',
			'access': 'party',
			'parents': 'party',
			'children': 'party',
			'volumes': 'volume',
			'sessions': 'slot',
			'assets': 'asset',
			'comments': 'comment'
		};

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

		var parseProps = function (object) {
			for (var prop in object) {
				if (!object.hasOwnProperty(prop) || !angular.isObject(object[prop]) || !hash[props[prop]])
					continue;

				var subCache = cacheFactory(props[prop]);

				angular.forEach(object[prop], function (subObject) {
					if (props[prop] === 'party')
						subCache.set(subObject.party, undefined, false);
					else
						subCache.set(subObject, undefined, false);
				});
			}console.log(hash);
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
						} else if (arrays[config.url] && arrays[config.url].length > 1) {
							return arrays[config.url];
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

					set: function (object, config, crawl) {
						if (thisCache.halt || !angular.isObject(object) || !object.id) {
							return false;
						}

						config = config || {};
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

						if (crawl !== false)
							parseProps(object);

						return true;
					},

					setArray: function (array, config) {
						if (!arrays[config.url])
							arrays[config.url] = [];

						for (var i = 0, l = array.length; i < l; i++) {
							thisCache.controller.set(array[i], config);

							if (arrays[config.url].indexOf(thisCache.cache[array[i].id].object) == -1) {
								arrays[config.url].push(thisCache.cache[array[i].id].object);
							}
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

		for (var prop in props) {
			if (props.hasOwnProperty(prop) && !hash[props[prop]]) {
				cacheFactory(props[prop]);
			}
		}

		return cacheFactory;
	}
]);
