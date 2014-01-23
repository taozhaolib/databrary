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
				active: true,
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

				$rootScope.$log.debug(browserService.options);
				$rootScope.$log.debug(browserService.data);
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
			angular.forEach(raw, function (volume) {
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

			loopRecursive(groups, 0, data.items, raw, []);

			browserService.data = data;
		};

		var loopRecursive = function (groups, level, current, data, ancestors) {
			var group = groups[level];

			if (['volume', 'session', 'asset'].indexOf(group) > -1)
				loopItemRecursive[group](groups, level, current, data, ancestors);
			else
				loopItemRecursive['record'](groups, level, current, data, ancestors);
		};

		var itemRecursive = function (groups, level, current, volume, ancestors) {
			var next = angular.extend({}, DEFAULT_GROUP, {
				object: ancestors[level],
				items: []
			});

			current.push(next);

			if (hasChildren(groups, level))
				loopRecursive(groups, level + 1, next.items, volume, ancestors);
		};

		var loopItemRecursive = {
			volume: function (groups, level, current, data, ancestors) {
				// for one or more volumes...
				angular.forEach(data, function (volume) {
					// never not the top level, if present
					ancestors = [volume];
					itemRecursive(groups, level, current, data, ancestors);
				});
			},

			record: function (groups, level, current, data, ancestors) {
				var group = groups[level];
				var volumes = {};

				// if there are no volumes, we need to iterate all records of this category
				if (ancestors.length > 0)
					volumes[ancestors[0].id] = ancestors[0];
				else
					volumes = data;

				var recordsAbove = groups.indexOf('volume') == -1 ? ancestors.slice(0, level) : ancestors.slice(1, level);

				// iterate one or more volumes
				angular.forEach(volumes, function (volume) {
					// iterate records in this category
					angular.forEach(volume.categories[group], function (recordID) {
						var good = true;

						// check if we need to cross index other records...
						if (recordsAbove.length != 0) {
							// get the sessions in this record...
							var sessions = angular.copy(volume.records[recordID].sessions);

							// iterate through cross-indexed records...
							angular.forEach(recordsAbove, function (recordAbove) {
								if (!$.isEmptyObject(sessions))
									// iterate through the current record's sessions...
									angular.forEach(sessions, function (session, sessionID) {
										// if this cross-record doesn't have this sessions, remove it
										if (!recordAbove.sessions.hasOwnProperty(sessionID))
											delete sessions[sessionID];
									});
							});

							if($.isEmptyObject(sessions))
								good = false;
						}

						// if there are sessions in this cross-indexed record...
						if (good) {
							ancestors = angular.copy(ancestors);
							ancestors.push(volume.records[recordID]);
							itemRecursive(groups, level, current, data, ancestors);
						}
					});
				});
			},

			session: function (groups, level, current, data, ancestors) {
				var sessions = {};

				if (level == 0) {
					angular.forEach(data, function (volume) {
						if (volume.sessions)
							angular.extend(sessions, volume.sessions);
					})
				} else if (groups[level - 1] == 'volume') {
					angular.extend(sessions, data.sessions);
				} else {
					var categoriesAbove = groups.indexOf('volume') == -1 ? groups.slice(0, level) : groups.slice(1, level);
				}
			},

			asset: function (groups, level, current, data, ancestors) {
			}
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
