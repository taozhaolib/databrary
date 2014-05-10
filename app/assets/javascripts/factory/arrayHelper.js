module.factory('arrayHelper', [
	'$filter', function ($filter) {
		return function (array) {
			var _transformFn, /* return formatted item */
				_validateFn, /* return item or false */
				_orderFn /* return standard ordering -/0/+ */;

			var catalog = {}, catalogKey;

			if (!angular.isArray(array)) {
				array = [];
			}

			//

			var newTransform = function (transformFn) {
				_transformFn = transformFn;
			};

			var newValidate = function (validateFn) {
				_validateFn = validateFn;
			};

			var newOrder = function (orderFn) {
				_orderFn = orderFn;
			};

			//

			var transform = function (item) {
				return angular.isFunction(_transformFn) ? _transformFn(item) : item;
			};

			var validate = function (item) {
				return angular.isFunction(_validateFn) ? _validateFn(item) : item;
			};

			var order = function () {
				return angular.isFunction(_orderFn) ? array.sort(_orderFn) : array;
			};

			//

			var newCatalog = function (key) {
				catalogKey = key;
				catalogUpdate();
			};

			var catalogUpdate = function () {
				catalog = {};

				if (angular.isString(catalogKey)) {
					angular.forEach(array, function (item, index) {
						catalogAdd(item);
					});
				}
			};

			var catalogAdd = function (item) {
				if (item[catalogKey]) {
					catalog[item[catalogKey]] = item;
				}
			};

			var catalogRemove = function (item) {
				if (item[catalogKey]) {
					delete catalog[item[catalogKey]];
				}
			};

			var useCatalog = function (item) {
				return catalogKey && !angular.isObject(item);
			};

			//

			var index = function (item) {
				if (useCatalog(item)) {
					item = catalog[item];
				}

				return array.indexOf(item);
			};

			var find = function (item, strict) {
				if (useCatalog(item)) {
					return catalog[item];
				}

				strict = angular.isUndefined(strict) ? true : strict;

				return filter(item, strict).shift();
			};

			var has = function (item) {
				if (useCatalog(item)) {
					return catalog[item];
				}

				return array[index(item)];
			};

			//

			var add = function (item) {
				if (!(item = validate(transform(item)))) {
					return false;
				}

				array.push(item);
				catalogAdd(item);

				order();

				return item;
			};

			var update = function (item, obj) {
				var i = index(item);

				if (!~i) {
					return undefined;
				}

				if (!angular.isObject(obj)) {
					return false;
				}

				catalogRemove(item);
				angular.extend(item, obj);
				catalogAdd(item);

				order();

				return item;
			};

			var replace = function (old, item) {
				if (!(item = validate(transform(item)))) {
					return false;
				}

				var i = index(old);

				if (!~i) {
					return undefined;
				}

				array[i] = item;
				catalogRemove(old);
				catalogAdd(item);

				order();

				return item;
			};

			var toggle = function (item, property, state) {
				var i = index(item);

				if (!~i) {
					return undefined;
				}

				var obj = {};

				obj[property] = angular.isDefined(state) ? state : !array[i][property];

				return update(item, obj);
			};

			var remove = function (item) {
				var i = index(item);

				if (!~i) {
					return undefined;
				}

				catalogRemove(item);
				return array.splice(i, 1).shift();
			};

			var filter = function (filter, comparator) {
				return !angular.isObject(filter) ? array : $filter('filter')(array, filter, comparator);
			};

			var reset = function () {
				return array.splice(0, array.length);
			};

			angular.extend(array, {
				index: index,
				validate: validate,
				has: has,
				add: add,
				update: update,
				replace: replace,
				remove: remove,
				filter: filter,
				find: find,
				order: order,
				toggle: toggle,
				reset: reset,
				newCatalog: newCatalog,
				newTransform: newTransform,
				newValidate: newValidate,
				newOrder: newOrder
			});

			return array;
		};
	}
]);
