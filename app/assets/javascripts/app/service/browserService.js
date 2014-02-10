define(['app/config/module'], function (module) {
	'use strict';

	module.factory('BrowserService', ['$rootScope', 'ArrayHelper', 'AuthService', function ($rootScope, arrayHelper, authService) {
		var browserService = {};

		//

		var DEFAULT_OPTIONS = {
			volume: {
				allow: false,
				active: true,
				expand: false,
				expanded: [],
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
				expand: false,
				expanded: [],
				filter: {},
				order: []
			}
		};

		var DEFAULT_CATEGORY = {
			id: null,
			name: null,
			allow: true,
			active: true,
			expand: false,
			expanded: null // array
		};

		//

		var raw = {};

		var contexts = ['search', 'party', 'volume'];
		var context = undefined;

		//

		browserService.options = {};

		browserService.data = {};

		//

		browserService.initialize = function (newContext, newData) {
			newData.$promise.then(function (newData) {
				browserService.initializeData(newData);
				browserService.initializeOptions(newContext);
				browserService.rebuildData();
			});
		};

		//

		browserService.initializeOptions = function (newContext) {
			if (contexts.indexOf(newContext) == -1)
				return false;

			context = newContext;

			angular.extend(browserService.options, DEFAULT_OPTIONS);

			browserService.updateCategories();
			browserService.options.record.categories.sort(function (a, b) {
				return a.id - b.id;
			});

			switch (context) {
				case 'party':
				case 'search':
					browserService.options.volume.allow = true;

					break;

				case 'volume':
					break;
			}

			return context;
		};

		browserService.updateCategories = function () {
			angular.forEach(raw, function (volume) {
				angular.forEach(volume.categories, function (sessions, category) {
					if (!browserService.options.record.categories.get({id: category}))
						browserService.options.record.categories.push(angular.extend({}, DEFAULT_CATEGORY, {
							id: category,
							name: $rootScope.constant.get('category', category).name,
							expanded: []
						}));
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

		browserService.rebuildData = function () {
			var groups = getActiveGroups(),
				data = {
					items: []
				};

			switch (groups[0]) {
				case 'volume':
					callbackVolumes(data, groups);
					break;

				case 'session':
					angular.forEach(raw, function (volume) {
						callbackSessions(data, volume);
					});
					break;

				default:
					angular.forEach(raw, function (volume) {
						callbackRecords(data, volume, groups, 0);
					});
					break;
			}

			browserService.data = data;

			return data;
		};

		browserService.updateData = function (levelData) {
			if (!levelData.object)
				return undefined;

			var type;

			if (browserService.isItemCategory(levelData.object.category)) {
				type = levelData.object.category;
			} else if (browserService.isItemType(levelData.type)) {
				type = levelData.type;
			}

			var groups = getActiveGroups(),
				level = groups.indexOf(type + '');

			if (level == -1)
				return undefined;

			switch (type) {
				case 'volume':
					callbackVolumeChildren(levelData, levelData.object, groups, level + 1);
					break;

				default:
					var currentVolume;

					angular.forEach(raw, function (volume) {
						if (angular.isUndefined(currentVolume) && volume.records[levelData.object.id])
							currentVolume = volume;
					});

					callbackRecordChildren(levelData, currentVolume, groups, level + 1);
					break;
			}

			return levelData;
		};

		//

		var isGroupActive = function (group) {
			return browserService.options[group].allow && browserService.options[group].active;
		};

		var getActiveGroups = function () {
			var groups = [];

			if (isGroupActive('volume'))
				groups.push('volume');

			groups.push.apply(groups, getActiveRecordGroups());

			if (isGroupActive('session'))
				groups.push('session');

			return groups;
		};

		var getActiveRecordGroups = function () {
			var groups = [];

			angular.forEach(browserService.options.record.categories, function (category) {
				if (category.allow && category.active)
					groups.push(category.id);
			});

			return groups;
		};

		//

		var callbackVolumes = function (data, groups) {
			angular.forEach(raw, function (volume, volumeID) {
				var newData = callbackItem(data, volume, volume.sessions, volume, 'volume');

				callbackVolumeChildren(newData, volume, groups, 1);
			});

			return data;
		};

		var callbackVolumeChildren = function (data, volume, groups, level) {
			if (browserService.options.volume.expanded.indexOf(volume.id) == -1)
				return data;

			level = level || 1;

			if (groups[level] == 'session')
				callbackSessions(data, volume);
			else
				callbackRecords(data, volume, groups, level);

			return data;
		};

		var callbackRecords = function (data, volume, groups, level) {
			var tempData = {};
			var sessions = data.sessions || volume.sessions;

			angular.forEach(sessions, function (session) {
				var categoryRecords = session.categories[groups[level]];

				if (angular.isDefined(categoryRecords)) {
					angular.forEach(categoryRecords, function (record, recordID) {
						recordID = record.id;

						if (!tempData[recordID])
							tempData[recordID] = {};

						tempData[recordID][session.id] = session;
					});
				}
			});

			if (!$.isEmptyObject(tempData)) {
				angular.forEach(tempData, function (newSessions, recordID) {
					var newData = callbackItem(data, volume, newSessions, volume.records[recordID], 'record');

					callbackRecordChildren(newData, volume, groups, level + 1);

					if (groups[level + 1] == 'session') {
						var c = 0;

						for (var key in newData.sessions) {
							if (newData.sessions.hasOwnProperty(key) && c++ == 1)
								break;
						}

						if (c <= 1)
							browserService.setItemExpand(newData, true);
					}
				});
			}

			return data;
		};

		var callbackRecordChildren = function (data, volume, groups, level) {
			if (!browserService.getItemExpand(data))
				return data;

			if (groups[level] == 'session')
				callbackSessions(data, volume);
			else
				callbackRecords(data, volume, groups, level);

			return data;
		};

		var callbackSessions = function (data, volume) {
			var sessions = data.sessions || volume.sessions;

			angular.forEach(sessions, function (session) {
				if (session.top)
					return;

				var newData = callbackItem(data, volume, undefined, session, 'session');
			});

			return data;
		};

		var callbackItem = function (data, volume, sessions, object, type) {
			var newData = {
				object: object,
				type: type,
				permission: object.permission || volume.permission,
				select: false,
				expand: false,
				items: [],
				parent: data,
				sessions: sessions
			};

			data.items.push(newData);

			return newData;
		};

		//

		browserService.isItemType = function (type) {
			return !!browserService.options[type];
		};

		browserService.isItemCategory = function (type) {
			return browserService.options.record.categories.index({id: type + ''}) > -1;
		};

		browserService.getItemType = function (object) {
			if (!angular.isObject(object))
				return undefined;

			if (browserService.isRecord(object))
				return 'record';

			if (browserService.isVolume(object))
				return 'volume';

			if (browserService.isAsset(object))
				return 'asset';

			return 'session';
		};

		browserService.isVolume = function (object) {
			return angular.isObject(object) && object.body;
		};

		browserService.isRecord = function (object) {
			return angular.isObject(object) && object.measures;
		};

		browserService.isAsset = function (object) {
			return angular.isObject(object) && object.asset;
		};

		browserService.isSession = function (object) {
			return angular.isObject(object) && !object.body && !object.measures;
		};

		//

		var recordGroupToggle = undefined;

		browserService.setRecordGroupToggle = function (group) {
			recordGroupToggle = angular.isUndefined(recordGroupToggle) ? group : undefined;
		};

		browserService.isRecordGroupToggle = function (group) {
			return recordGroupToggle == group;
		};

		//

		browserService.setGroupActive = function (type, active) {
			if (!browserService.isItemType(type))
				return undefined;

			browserService.options[type].active =
				angular.isUndefined(active) ?
					!browserService.options[type].active : !!active;

			browserService.rebuildData();

			return true;
		};

		browserService.canAddRecordGroup = function () {
			var canAdd = false;

			angular.forEach(browserService.options.record.categories, function (recordGroup) {
				if (!canAdd && !recordGroup.active) {
					canAdd = true;
				}
			});

			return canAdd;
		};

		browserService.addRecordGroup = function () {
			var go = true;

			angular.forEach(browserService.options.record.categories, function (recordGroup) {
				if (go && !recordGroup.active) {
					recordGroup.active = true;
					go = false;
				}
			});

			browserService.rebuildData();
		};

		browserService.canRemoveRecordGroup = function () {
			return true;
		};

		browserService.removeRecordGroup = function (group) {
			group.active = false;

			// move to end
			var group_i = browserService.options.record.categories.index(group);

			browserService.options.record.categories.splice(group_i, 1);
			browserService.options.record.categories.push(group);

			browserService.rebuildData();
		};

		browserService.switchRecordGroup = function (group, maybe) {
			browserService.setRecordGroupToggle(undefined);

			var group_i = browserService.options.record.categories.index(group),
				maybe_i = browserService.options.record.categories.index(maybe);

			if (group.active != maybe.active) {
				group.active = !group.active;
				maybe.active = !maybe.active;
			}

			browserService.options.record.categories[group_i] = browserService.options.record.categories.splice(maybe_i, 1, browserService.options.record.categories[group_i])[0];

			browserService.rebuildData();
		};

		//

		browserService.setItemExpand = function (levelData, expand) {
			if (!levelData.expand && expand !== false) {
				levelData.expand = true;
				browserService.updateData(levelData);
			} else if (levelData.expand && expand !== true) {
				levelData.expand = false;
			}

			return levelData;
		};

		browserService.getItemExpand = function (levelData) {
			return levelData.expand;
		};

		//

		browserService.getSorts = function (data, active) {

		};

		var sortToggle = undefined;

		browserService.setSortToggle = function (sort) {
			sortToggle = angular.isUndefined(sortToggle) ? sort : undefined;
		};

		browserService.isSortToggle = function (sort) {
			return sortToggle == sort;
		};

		browserService.switchSort = function (sort, maybe) {

		};

		browserService.canReverseSort = function () {

		};

		browserService.reverseSort = function (sort) {

		};

		browserService.canRemoveSort = function () {

		};

		browserService.removeSort = function (sort) {

		};

		browserService.canAddSort = function () {

		};

		browserService.addSort = function (sort) {

		};

		//

		browserService.getObjectPermission = function (object) {
			var type = browserService.getItemType(object),
				permission = undefined;

			angular.forEach(raw, function (volume) {
				switch (type) {
					case 'volume':
						if (volume.id == object.id)
							permission = volume.permission;
						break;

					case 'session':
						if (volume.sessions.indexOf(object.id) > -1)
							permission = volume.permission;
						break;

					case 'record':
						if (volume.records.indexOf(object.id) > -1)
							permission = volume.permission;
						break;

					case 'asset':
						permission = object.permission;
						break;
				}
			});

			return permission;
		};

		browserService.hasAccess = function (object, level) {
			return object >= level || authService.hasAuth('SUPER');
		};

		//

		var itemSelect = undefined;

		browserService.setItemSelect = function (data) {
			if (angular.isDefined(itemSelect))
				itemSelect.select = false;

			if (itemSelect == data)
				return itemSelect = undefined;

			data.select = true;
			return itemSelect = data;
		};

		browserService.getItemSelect = function () {
			return itemSelect;
		};

		browserService.isItemSelect = function (data) {
			return data = itemSelect;
		};

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

		return browserService;
	}]);
});
