module.factory('ArrayHelper', [
	'$filter', function ($filter) {
		var ArrayHelper = function (array) {
			if (!angular.isArray(array)) {
				array = [];
			}

			array.unshift(0, this.length);
			Array.prototype.splice.apply(this, array);

			this._transformFn = undefined /* return formatted item */;
			this._validateFn = undefined /* return item or false */;
			this._orderFn = undefined /* return standard ordering -/0/+ */;

			this.catalog = {};
			this.catalogKey = undefined;
		};

		ArrayHelper.prototype = [];

		//

		ArrayHelper.prototype.newTransform = function (transformFn) {
			this._transformFn = transformFn;
		};
		ArrayHelper.prototype.newValidate = function (validateFn) {
			this._validateFn = validateFn;
		};
		ArrayHelper.prototype.newOrder = function (orderFn) {
			this._orderFn = orderFn;
		};

		//

		ArrayHelper.prototype.transform = function (item) {
			return angular.isFunction(this._transformFn) ? this._transformFn(item) : item;
		};
		ArrayHelper.prototype.validate = function (item) {
			return angular.isFunction(this._validateFn) ? this._validateFn(item) : item;
		};
		ArrayHelper.prototype.order = function () {
			return angular.isFunction(this._orderFn) ? this.sort(this._orderFn) : this;
		};

		//


		ArrayHelper.prototype.newCatalog = function (key) {
			this.catalogKey = key;
			this.catalogUpdate();
		};
		ArrayHelper.prototype.catalogUpdate = function () {
			this.catalog = {};

			if (angular.isString(this.catalogKey)) {
				var that = this;
				angular.forEach(this, function (item) {
					that.catalogAdd(item);
				});
			}
		};
		ArrayHelper.prototype.catalogAdd = function (item) {
			if (item[this.catalogKey]) {
				this.catalog[item[this.catalogKey]] = item;
			}
		};
		ArrayHelper.prototype.catalogRemove = function (item) {
			if (item[this.catalogKey]) {
				delete this.catalog[item[this.catalogKey]];
			}
		};
		ArrayHelper.prototype.useCatalog = function (item) {
			return this.catalogKey && !angular.isObject(item);
		};

		//


		ArrayHelper.prototype.index = function (item) {
			if (this.useCatalog(item)) {
				item = this.catalog[item];
			}

			return this.indexOf(item);
		};
		ArrayHelper.prototype.find = function (item, strict) {
			if (this.useCatalog(item)) {
				return this.catalog[item];
			}

			strict = angular.isUndefined(strict) ? true : strict;

			return this.filter(item, strict).shift();
		};
		ArrayHelper.prototype.has = function (item) {
			if (this.useCatalog(item)) {
				return this.catalog[item];
			}

			return this[this.index(item)];
		};
		ArrayHelper.prototype.add = function (item) {
			if (!(item = this.validate(this.transform(item)))) {
				return false;
			}

			this.push(item);
			this.catalogAdd(item);

			this.order();

			return item;
		};
		ArrayHelper.prototype.update = function (item, obj) {
			var i = this.index(item);

			if (!~i) {
				return undefined;
			}

			if (!angular.isObject(obj)) {
				return false;
			}

			this.catalogRemove(item);
			angular.extend(item, obj);
			this.catalogAdd(item);

			this.order();

			return item;
		};
		ArrayHelper.prototype.replace = function (old, item) {
			if (!(item = this.validate(this.transform(item)))) {
				return false;
			}

			var i = this.index(old);

			if (!~i) {
				return undefined;
			}

			this[i] = item;
			this.catalogRemove(old);
			this.catalogAdd(item);

			this.order();

			return item;
		};
		ArrayHelper.prototype.toggle = function (item, property, state) {
			var i = this.index(item);

			if (!~i) {
				return undefined;
			}

			var obj = {};

			obj[property] = angular.isDefined(state) ? state : !this[i][property];

			return ArrayHelper.prototype.update.call(this, item, obj);
		};
		ArrayHelper.prototype.remove = function (item) {
			var i = this.index(item);

			if (!~i) {
				return undefined;
			}

			this.catalogRemove(item);
			return this.splice(i, 1).shift();
		};
		ArrayHelper.prototype.filter = function (filter, comparator) {
			return !angular.isObject(filter) ? this : $filter('filter')(this, filter, comparator);
		};
		ArrayHelper.prototype.reset = function () {
			return this.splice(0, this.length);
		};

		//

		return ArrayHelper;
	}
]);
