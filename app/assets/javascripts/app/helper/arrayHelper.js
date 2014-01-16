define(['app/config/module'], function (module) {
	'use strict';

	module.factory('ArrayHelper', ['$filter', function ($filter) {
		return function (array, transformFn, validationFn, orderFn) {
			var _transformFn,
				_validationFn,
				_orderFn;

			if (!angular.isArray(array))
				array = [];

			var newTransform = function (transformFn) {
				_validationFn = transformFn;
			};

			var newValidation = function (validationFn) {
				_validationFn = validationFn;
			};

			var newOrder = function (orderFn) {
				_orderFn = orderFn;
			};

			var index = function (item) {
				if (!angular.isFunction(_validationFn) || !validate(item))
					item = $filter('filter')(array, item, true).shift();

				return array.indexOf(item);
			};

			var transform = function (item) {
				return angular.isFunction(_transformFn) ? _transformFn(item) : item;
			};

			var validate = function (item) {
				return angular.isFunction(_validationFn) ? _validationFn(item) : item;
			};

			var order = function () {
				return angular.isFunction(_orderFn) ? array.sort(_orderFn) : array;
			};

			var get = function (item) {
				return array[index(item)];
			};

			var add = function (item) {
				if (!(item = validate(transform(item))))
					return false;

				array.push(item);

				order();

				return item;
			};

			var update = function (old, item) {
				var i = index(old);

				if (!~i)
					return undefined;

				if (!(item = validate(angular.extend({}, array[i], transform(item)))))
					return false;

				return replace(old, item);
			};

			var replace = function (old, item) {
				if (!(item = validate(transform(item))))
					return false;

				var i = index(old);

				if (!~i)
					return undefined;

				array[i] = item;

				order();

				return item;
			};

			var toggle = function (item, property, state) {
				var i = index(item);

				if (!~i)
					return undefined;

				if(angular.isUndefined(property))
					return update(item, !array[i]);

				var obj = {};

				obj[property] = angular.isDefined(state) ? state : !array[i][property];

				return update(item, obj);
			};

			var echo = function (item, callback) {
				var i = index(item);

				if (!~i || !angular.isFunction(callback))
					return undefined;

				return callback(array[i]);
			};

			var remove = function (item) {
				var i = index(item);

				if (!~i)
					return undefined;

				return array.splice(i, 1).shift();
			};

			var filter = function (filter, comparator) {
				return angular.isUndefined(filter) ? array : $filter('filter')(array, filter, comparator);
			};

			var all = function () {
				return array;
			};

			var reset = function () {
				return array.splice(0, array.length);
			};

			var methods = {
				index: index,
				validate: validate,
				get: get,
				echo: echo,
				add: add,
				update: update,
				replace: replace,
				remove: remove,
				filter: filter,
				order: order,
				toggle: toggle,
				all: all,
				reset: reset,
				newTransform: newTransform,
				newValidation: newValidation,
				newOrder: newOrder
			};

			angular.extend(array, methods);

			newTransform(transformFn);
			newValidation(validationFn);
			newOrder(orderFn);

			return array;
		};
	}]);
});
