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

//				filter: {},
				sort: arrayHelper([])
			},

			record: {
				allow: true,
				categories: arrayHelper([])
			},

			session: {
				allow: true,
				active: true,
				expand: false,

//				filter: {},
				sort: arrayHelper([])
			}
		};

		var DEFAULT_CATEGORY = {
			id: null,
			name: null,

			allow: true,
			active: true,
			expand: false,

//			filter: {},
			sort: null // see updateCategories
		};

		var DEFAULT_SORT = {
			name: null,
			property: null,

			active: false,
			order: true
		};

		//

		var raw = [];

		var contexts = ['search', 'party', 'volume'];
		var context = undefined;

		//

		browserService.options = {};

		browserService.data = {};

		browserService.groups = {};

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

			browserService.updateSorts();

			browserService.updateCategories();
			browserService.options.record.categories.sort(function (a, b) {
				return a.id - b.id;
			});

			switch (context) {
				case 'party':
				case 'search':
					browserService.options.volume.allow = true;
					browserService.options.session.allow = false;

					break;

				case 'volume':
					browserService.options.volume.allow = false;
					browserService.options.session.allow = true;
					break;
			}

			return context;
		};

		browserService.updateSorts = function () {
			browserService.updateVolumeSorts();
			// TODO: record sorts...
			browserService.updateSessionSorts();
		};

		browserService.updateVolumeSorts = function () {
			var option = browserService.options.volume;

			if (option)
				option.sort.length = 0;

			option.sort.push(angular.extend({}, DEFAULT_SORT, {
				name: 'Name',
				property: ['name']
			}));

			option.sort.push(angular.extend({}, DEFAULT_SORT, {
				name: 'Creation Date',
				property: ['creation']
			}));
		};

		browserService.updateSessionSorts = function () {
			var option = browserService.options.session;

			if (option)
				option.sort.length = 0;

			option.sort.push(angular.extend({}, DEFAULT_SORT, {
				name: 'Consent',
				property: ['consent']
			}));

			option.sort.push(angular.extend({}, DEFAULT_SORT, {
				name: 'Date',
				property: ['date']
			}));

			option.sort.push(angular.extend({}, DEFAULT_SORT, {
				name: 'Age',
				property: ['age']
			}));
		};

		browserService.updateCategories = function () {
			if (browserService.options.record)
				browserService.options.record.categories.length = 0;

			angular.forEach(raw, function (volume) {
				angular.forEach(volume.categories, function (sessions, category) {
					if (!browserService.options.record.categories.get({id: category}))
						browserService.options.record.categories.push(angular.extend({}, DEFAULT_CATEGORY, {
							id: category,
							name: $rootScope.constant.get('category', category).name,
							sort: arrayHelper([])
						}));
				});
			});
		};

		browserService.getContext = function () {
			return context;
		};

		//

		browserService.initializeData = function (newData) {
			if (newData.id)
				raw = [newData];
			else if (angular.isArray(newData))
				raw = newData;
		};

		browserService.rebuildData = function () {
			var groups = getActiveGroups(),
				data = {
					items: [],
					level: -1,
					group: 'browser'
				};

			browserService.groups = {};

			angular.forEach(groups, function (group) {
				browserService.groups[group] = [];
			});

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
						callbackRecords(data, volume, groups);
					});
					break;
			}

			browserService.data = data;

			return data;
		};

		browserService.updateData = function (data) {
			if (!data.object)
				return undefined;

			var groups = getActiveGroups();

			if (!groups[data.level])
				return undefined;

			switch (data.group) {
				case 'volume':
					callbackVolumeChildren(data, data.object, groups);
					break;

				default:
					callbackRecordChildren(data, data.volume, groups);
					break;
			}

			return data;
		};

		browserService.filterDataGroup = function (level) {
			var groups = getActiveGroups(),
				parent = groups[level],
				children = groups[level + 1],
				sortables, filterables;

			if (!children)
				return;

			if(parent)
				sortables = browserService.groups[parent];
			else
				sortables = [browserService.data];

			filterables = browserService.groups[children];

			angular.forEach(sortables, function (data) {
				sortItems(data, children);
			});

			angular.forEach(filterables, function (data) {
				// TODO: filter
				// adjust data.active
			});
		};

		var sortItems = function (data, group) {
			var option = getOption(data, true),
				length = option.sort.length;

			for (var i = length - 1; i >= 0; i--) {
				switch(group) {
					case 'volume':
						sortVolumes(data, option.sort[i]);
						break;

					case 'session':
						sortSessions(data, option.sort[i]);
						break;

					default:
						sortRecords(data, option.sort[i]);
						break;
				}
			}
		};

		var sortVolumes = function (data, sort) {

		};

		var sortRecords = function (data, sort) {

		};

		var sortSessions = function (data, sort) {
			switch (sort.name) {
				default:
					// if property exists, sort array callback...
					break;
			}
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

		browserService.showList = function (data) {
			return !!getActiveGroups()[data.level + 1];
		};

		//

		var callbackVolumes = function (data, groups) {
			angular.forEach(raw, function (volume) {
				if (volume.id) {
					var newData = callbackItem(data, volume, volume.sessions, volume, 'volume');

					callbackVolumeChildren(newData, volume, groups);
				}
			});

			return data;
		};

		var callbackVolumeChildren = function (data, volume, groups) {
			if (!browserService.getItemExpand(data))
				return data;

			if (groups[data.level + 1] == 'session')
				callbackSessions(data, volume);
			else
				callbackRecords(data, volume, groups);

			return data;
		};

		var callbackRecords = function (data, volume, groups) {
			var tempData = {};
			var sessions = data.sessions || volume.sessions;

			angular.forEach(sessions, function (session) {
				var categoryRecords = session.categories[groups[data.level + 1]];

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
					var newData = callbackItem(data, volume, newSessions, volume.records[recordID], volume.records[recordID].category);

					callbackRecordChildren(newData, volume, groups);

					if (groups[newData.level + 1] == 'session') {
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

		var callbackRecordChildren = function (data, volume, groups) {
			if (!browserService.getItemExpand(data))
				return data;

			if (groups[data.level + 1] == 'session')
				callbackSessions(data, volume);
			else
				callbackRecords(data, volume, groups);

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

		var callbackItem = function (data, volume, sessions, object, group) {
			var newData = {
				parent: data,
				volume: volume,
				sessions: sessions,
				level: data.level + 1,

				object: object,
				permission: object.permission || volume.permission,
				group: group,
				items: [],

				select: false,
				expand: false
			};

			browserService.groups[group].push(newData);
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

			if (browserService.isParty(object))
				return 'party';

			return 'session';
		};

		browserService.isAsset = function (object) {
			return angular.isObject(object) && object.asset;
		};

		browserService.isVolume = function (object) {
			return angular.isObject(object) && object.body;
		};

		browserService.isRecord = function (object) {
			return angular.isObject(object) && object.measures;
		};

		browserService.isParty = function (object) {
			return angular.isObject(object) && object.avatar;
		};

		browserService.isSession = function (object) {
			return angular.isObject(object) && !object.body && !object.measures && !object.avatar;
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

		browserService.setItemExpand = function (data, expand) {
			if (!data.expand && expand !== false) {
				data.expand = true;

				if (data.items == 0)
					browserService.updateData(data);
			} else if (data.expand && expand !== true) {
				data.expand = false;
			}

			return data;
		};

		browserService.getItemExpand = function (data) {
			return data.expand;
		};

		browserService.canExpand = function (data) {
			return data.level >= 0 && getActiveGroups()[data.level + 1];
		};

		//

		var getOption = function (data, child) {
			var level = child === true ? data.level + 1 : data.level,
				group = getActiveGroups()[level];

			switch (group) {
				case 'session':
					return browserService.options.session;

				case 'volume':
					return browserService.options.volume;

				default:
					return browserService.options.record.categories.find({id: group});
			}
		};

		browserService.getSorts = function (data) {
			return getOption(data, true).sort;
		};

		var sortToggle = undefined;

		browserService.setSortToggle = function (sort) {
			sortToggle = angular.isUndefined(sortToggle) ? sort : undefined;
		};

		browserService.isSortToggle = function (sort) {
			return sortToggle == sort;
		};

		browserService.switchSort = function (data, sort, maybe) {
			browserService.setSortToggle(undefined);

			var option = getOption(data, true);

			var sort_i = option.sort.index(sort),
				maybe_i = option.sort.index(maybe);

			if (sort.active != maybe.active) {
				sort.active = !sort.active;
				maybe.active = !maybe.active;
			}

			option.sort[sort_i] = option.sort.splice(maybe_i, 1, option.sort[sort_i])[0];

			browserService.filterDataGroup(data.level);
		};

		browserService.canReverseSort = function () {
			return true;
		};

		browserService.reverseSort = function (data, sort) {
			sort.order = !sort.order;

			browserService.filterDataGroup(data.level);
		};

		browserService.canRemoveSort = function () {
			return true;
		};

		browserService.removeSort = function (data, sort) {
			sort.active = false;

			var option = getOption(data, true);

			// move to end
			var sort_i = option.sort.index(sort);

			option.sort.splice(sort_i, 1);
			option.sort.push(sort);

			browserService.filterDataGroup(data.level);
		};

		browserService.canAddSort = function (data) {
			var canAdd = false;

			var option = getOption(data, true);

			angular.forEach(option.sort, function (sort) {
				if (!canAdd && !sort.active)
					canAdd = true;
			});

			return canAdd;
		};

		browserService.addSort = function (data) {
			var go = true;

			var option = getOption(data, true);

			angular.forEach(option.sort, function (sort) {
				if (go && !sort.active) {
					sort.active = true;
					go = false;
				}
			});

			browserService.filterDataGroup(data.level);
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
