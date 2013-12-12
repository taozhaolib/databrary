define(['app/config/module'], function (module) {
	'use strict';

	module.factory('AuthService', ['$filter', function ($filter) {
		var arrayHelper = function (array) {
			return {
				index: function (field, value) {

				},
				indexBy: function (value, field) {

				},

				get: function (item) {
					return array[this.index(item)];
				},
				create: function (item) {
					return array.push(item);
				},
				update: function (old, item) {
					var i = this.index(old);

					if (!~i)
						return undefined;

					return angular.extend(array[i], item);
				},
				replace: function (old, item) {
					var i = this.index(old);

					if (!~i)
						return undefined;

					array[i] = item;

					return item;
				},
				remove: function (item) {
					var i = this.index(old);

					if (!~i)
						return undefined;

					// TODO

				},

				query: function (filter, comparator) {
					if (angular.isUndefined(filter))
						return array;
					else
						return $filter('filter')(array, filter, comparator);
				}
			};
		};

		//

		return arrayHelper;
	}]);
});
