define(['app/config/module'], function (module) {
	'use strict';

	module.factory('BrowserService', ['$rootScope', 'ArrayHelper', function ($rootScope, arrayHelper) {
		var browserService = {};

		//

		var DEFAULT_OPTIONS = {
			volume: {
				allow: true,
				active: true,
				expand: true,
				filter: {},
				order: []
			},
			record: {
				allow: true,
				filter: {},
				order: [],
				categories: arrayHelper([])
			},
			session: {
				allow: true,
//				active: true,
				active: false,
				expand: true,
				filter: {},
				order: []
			},
			asset: {
				allow: true,
//				active: true,
				active: false,
				expand: true,
				filter: {},
				order: []
			}
		};

		var DEFAULT_CATEGORY = {
			name: null,
			allow: true,
//			active: false,
			active: true,
			expand: true
		};

		var DEFAULT_DATA = {
//			expand: true,
			items: []
		};

		var DEFAULT_GROUP = {
			object: null,
//			expand: false,
			items: []
		};

		//

		var raw = {};

		var contexts = ['search', 'party', 'volume', 'record', 'session', 'asset'];
		var context = undefined;

		//

		browserService.options = {};

		browserService.data = {};

		//

		browserService.initialize = function (newContext, newData) {
			newData.$promise.then(function (newData) {
				browserService.initializeData(newData);
				browserService.initializeOptions(newContext);
				browserService.updateData();
			});
		};

		//

		browserService.initializeOptions = function (newContext) {
			if (contexts.indexOf(newContext) == -1)
				return false;

			context = newContext;

			angular.extend(browserService.options, DEFAULT_OPTIONS);

			browserService.updateCategories();

			switch (context) {

				case 'volume':
				case 'record':
				case 'session':
				case 'asset':
					browserService.options.volume.allow = false;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

				case 'party':
				case 'search':
					browserService.options.volume.allow = true;
			}

			var participant = browserService.options.record.categories.get({name: 'participant'});
			if (participant)
				participant.active = true;

			return context;
		};

		browserService.updateCategories = function () {
			angular.forEach(raw, function (volume, id) {
				angular.forEach(volume.categories, function (sessions, category) {
					if (!browserService.options.record.categories.get({name: category}))
						browserService.options.record.categories.push(angular.extend({}, DEFAULT_CATEGORY, {name: category}));
				});
			});
		};

		browserService.getContext = function () {
			return context;
		};

		//

		browserService.initializeData = function (newData) {
			raw = {};

			if (newData.id)
				raw[newData.id] = newData;
			else if (angular.isObject(newData))
				raw = newData;
		};

		browserService.updateData = function () {
			var groups = getActiveGroups();

			var data = angular.extend({}, DEFAULT_DATA);

			if (groups.length == 0)
				return;

			loopRecursive(groups, 0, data.items, raw);

			browserService.data = data;
		};

		var loopRecursive = function (groups, level, current, data) {
			console.log('loopRecursive');

			var group = groups[level];

			if (['volume', 'session', 'asset'].indexOf(group) > -1)
				loopItemRecursive[group](groups, level, current, data);
			else
				loopItemRecursive['record'](groups, level, current, data);
		};

		var itemRecursive = function (groups, level, current, volume, object) {
			console.log('itemRecursive', current.length);

			var group = groups[level];

			var next = angular.extend({}, DEFAULT_GROUP, {
				object: object,
				items: []
			});

			current.push(next);

			if (hasChildren(groups, level))
				loopRecursive(groups, level + 1, next.items, volume);
		};

		var loopItemRecursive = {
			volume: function (groups, level, current, volumes) {
				console.log('loopRecursive volume');

				angular.forEach(volumes, function (volume) {
					itemRecursive(groups, level, current, volume, volume);
				});
			},

			record: function (groups, level, current, volume) {
				console.log('loopRecursive record');

				var records = [];
				var group = groups[level];

				var categoriesAbove = groups.indexOf('volume') == -1 ? groups.slice(0, level) : groups.slice(1, level);

				angular.forEach(volume.categories[group], function (record) {
					var goodOne = categoriesAbove.length == 0;

					angular.forEach(categoriesAbove, function (categoryAbove) {
						if (!goodOne)
							angular.forEach(volume.categories[categoryAbove], function (recordAbove) { console.log(record, recordAbove);
								goodOne = intersectRecords(volume, volume.records[record], volume.records[recordAbove])
							});
					});

					if (goodOne)
						itemRecursive(groups, level, current, volume, volume.records[record]);
				});
			},

			session: function (groups, level, current, volume) {
			},

			asset: function (groups, level, current, volume) {
			}
		};

		var intersectRecords = function (volume, recordA, recordB) {
			var keyA = 0, keyB = 0;

			var sessionsA = Object.keys(recordA.sessions).sort();
			var sessionsB = Object.keys(recordB.sessions).sort();

			while (keyA < sessionsA.length && keyB < sessionsB.length) {
				if (sessionsA[keyA] < sessionsB[keyB]) {
					keyA++;
				} else if (sessionsA[keyA] > sessionsB[keyB]) {
					keyB++;
				} else {
					return true;
				}
			}

			return false;
		};

		var hasChildren = function (groups, level) {
			return groups.length != level + 1;
		};

		var getActiveGroups = function () {
			var groups = [];

			if (isGroupActive('volume'))
				groups.push('volume');

			if (browserService.options.record)
				angular.forEach(browserService.options.record.categories, function (category) {
					if (category.allow && category.active)
						groups.push(category.name);
				});

			if (isGroupActive('session'))
				groups.push('session');

			if (isGroupActive('asset'))
				groups.push('asset');

			return groups;
		};

		var isGroupActive = function (group) {
			return browserService.options[group].allow && browserService.options[group].active;
		};

		//

//		browserService.expandItem = function () {
//			// if volume and no items,
//			// browserService.expandVolume() .then expandItem()
//
//			//
//		};
//
//		browserService.expandVolume = function () {
//
//		};

		//

		$rootScope.$watch(function () {
			var fullCount = 0;

			angular.forEach(raw, function (volume) {
				if (volume.full)
					fullCount++;
			});

			return fullCount;
		}, function () {
			browserService.updateCategories();
		});

		//


		//

//		browserService.getSortLevels = function () {
//			var levels = [];
//
//			if (browserService.sort.volume.active)
//				levels.push('volume');
//
//			if (browserService.sort.record.allow) {
//				angular.forEach(browserService.recordSorts, function (sort) {
//					if (sort.active)
//						levels.push(sort.name);
//				});
//			}
//
//			if (browserService.sort.session.active)
//				levels.push('session');
//
//			if (browserService.sort.asset.active)
//				levels.push('asset');
//
//			return levels;
//		};
//
//		browserService.getLevelType = function (depth) {
//			if (!angular.isNumber(depth))
//				return undefined;
//
//			return browserService.getSortLevels()[depth];
//		};
//
//		browserService.getLevelItems = function (depth, args) {
//			return [];
//		};
//
//		browserService.hasLevelItems = function (depth, args) {
//			return browserService.getLevelItems(depth, args).length > 0;
//		};

		//

		return browserService;
	}]);
});
