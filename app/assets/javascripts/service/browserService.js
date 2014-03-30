define(['config/module'], function (module) {
	'use strict';

	module.factory('BrowserService', ['$rootScope', 'ArrayHelper', 'AuthService', 'Slot', 'TypeService', 'PageService', function ($rootScope, arrayHelper, authService, Slot, typeService, page) {
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
			},

			asset: {
				allow: true,
				active: false,
				expand: false,

//				filter: {},
				sort: arrayHelper([])
			}
		};

		var DEFAULT_CATEGORY = {
			id: null,
			name: null,

			allow: true,
			active: false,
			expand: true,

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
		browserService.context = undefined;

		//

		browserService.options = {};

		browserService.data = {};

		browserService.groups = {};

		//

		browserService.initialize = function (newContext, newData) {
			if (newContext == 'party') {
				var volumes = [];

				angular.forEach(newData, function (access) {
					volumes.push(access.volume);
				});

				return initialize(newContext, volumes);
			}

			newData.$promise.then(function (newData) {
				initialize(newContext, newData);
			});
		};

		var initialize = function (newContext, newData) {
			browserService.initializeData(newData);
			browserService.initializeOptions(newContext);
			browserService.rebuildData();
		};

		//

		browserService.initializeOptions = function (newContext) {
			if (contexts.indexOf(newContext) == -1)
				return false;

			browserService.context = newContext;

			angular.extend(browserService.options, DEFAULT_OPTIONS);

			browserService.updateSorts();

			browserService.updateCategories();
			browserService.options.record.categories.sort(function (a, b) {
				return a.id - b.id;
			});

			switch (browserService.context) {
				case 'party':
				case 'search':
					browserService.options.volume.allow = true;
					browserService.options.session.allow = false;
					browserService.options.asset.allow = false;

					break;

				case 'volume':
					browserService.options.volume.allow = false;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;
					browserService.options.asset.active = true;
					break;
			}

			return browserService.context;
		};

		browserService.updateSorts = function () {
			browserService.updateVolumeSorts();
			// TODO: record sorts...
			browserService.updateSessionSorts();
			// TODO: asset sorts...
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
					if (!browserService.options.record.categories.find({id: category}))
						browserService.options.record.categories.push(angular.extend({}, DEFAULT_CATEGORY, {
							id: category,
							name: $rootScope.constant.get('category', category).name,
							sort: arrayHelper([])
						}));
				});
			});
		};

		//

		browserService.initializeData = function (newData) {
			if (newData.id)
				raw = [newData];
			else if (angular.isArray(newData))
				raw = newData;
		};

		var focus, focusInvert, focusPosition;

		browserService.rebuildData = function (focusGroup) {
			if (focusGroup) {
				focusInvert = (focus == focusGroup && getLevelByGroup(focusGroup.id) == focusPosition) ? !focusInvert : undefined;
				focusPosition = getLevelByGroup(focusGroup.id);
			}

			focus = focusGroup;

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
						callbackSessions(data, volume, groups);
					});
					break;

				case 'asset':
					angular.forEach(raw, function (volume) {
						callbackAssets(data, volume);
					});
					break;

				default:
					angular.forEach(raw, function (volume) {
						callbackRecords(data, volume, groups);
					});
					break;
			}

			angular.extend(browserService.data, data);

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

				case 'session':
					callbackSessionChildren(data, data.volume, groups);
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

			if (parent)
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
				switch (group) {
					case 'volume':
						sortVolumes(data, option.sort[i]);
						break;

					case 'session':
						sortSessions(data, option.sort[i]);
						break;

					case 'asset':
						sortAssets(data, option.sort[i]);
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

		var sortAssets = function (data, sort) {

		};

		//

		var isGroupAllowed = function (group) {
			return browserService.options[group] && browserService.options[group].allow;
		};

		var isGroupActive = function (group) {
			return isGroupAllowed(group) && browserService.options[group].active;
		};

		var getActiveGroups = function () {
			var groups = [];

			if (isGroupActive('volume'))
				groups.push('volume');

			groups.push.apply(groups, getActiveRecordGroups());

			if (isGroupActive('session'))
				groups.push('session');

			if (isGroupActive('asset'))
				groups.push('asset');

			return groups;
		};

		browserService.getActiveGroups = getActiveGroups;

		browserService.getFilterGroups = function () {
			var groups = getActiveGroups(), output = [];

			angular.forEach(groups, function (group) {
				output.push(browserService.groups[group]);
			});
		};

		var getAllowedGroups = function () {
			var groups = [];

			if (isGroupAllowed('volume'))
				groups.push('volume');

			groups.push.apply(groups, getAllowedRecordGroups());

			if (isGroupAllowed('session'))
				groups.push('session');

			if (isGroupAllowed('asset'))
				groups.push('asset');

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

		var getAllowedRecordGroups = function () {
			var groups = [];

			angular.forEach(browserService.options.record.categories, function (category) {
				if (category.allow)
					groups.push(category.id);
			});

			return groups;
		};

		browserService.isLastGroup = function (group) {
			var groups = getAllowedGroups();

			return groups.indexOf(group) == groups.length - 1;
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
				callbackSessions(data, volume, groups);
			else if (groups[data.level + 1] == 'asset')
				callbackAssets(data, volume);
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
					angular.forEach(categoryRecords, function (record) {
						if (!tempData[record.id])
							tempData[record.id] = {};

						tempData[record.id][session.id] = session;
					});
				} else {
					if (!tempData['null'])
						tempData['null'] = {};

					tempData['null'][session.id] = session;
				}
			});

			if (!$.isEmptyObject(tempData)) {
				angular.forEach(tempData, function (newSessions, recordID) {
					var newData;

					if (volume.records[recordID])
						newData = callbackItem(data, volume, newSessions, volume.records[recordID], groups[data.level + 1]);
					else
						newData = callbackItem(data, volume, newSessions, {
							category: groups[data.level + 1],
							id: 0,
							measures: {}
						}, groups[data.level + 1]);

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

				data.items.reverse();
			}

			return data;
		};

		var callbackRecordChildren = function (data, volume, groups) {
			if (!browserService.getItemExpand(data))
				return data;

			if (groups[data.level + 1] == 'session')
				callbackSessions(data, volume, groups);
			else if (groups[data.level + 1] == 'asset')
				callbackAssets(data, volume);
			else
				callbackRecords(data, volume, groups);

			return data;
		};

		var callbackSessions = function (data, volume, groups) {
			var sessions = data.sessions || volume.sessions;

			angular.forEach(sessions, function (session) {
				var newData = callbackItem(data, volume, undefined, session, 'session');

				callbackSessionChildren(newData, volume, groups);
			});

			return data;
		};

		var callbackSessionChildren = function (data, volume, groups) {
			if (!browserService.getItemExpand(data))
				return data;

			if (groups[data.level + 1] == 'asset')
				callbackAssets(data, volume);

			return data;
		};

		var callbackAssets = function (data, volume) {
			var sessions = data.sessions || volume.sessions;

			Slot.get({
				id: data.object.id,
				segment: data.object.segment || ',',
				assets: ''
			}, function (object) {
				angular.forEach(object.assets, function (asset) {
					asset.container = object.container;
					asset.segment = object.segment;
					var newData = callbackItem(data, volume, undefined, asset, 'asset');
				});
			});

			return data;
		};

		var callbackItem = function (data, volume, sessions, object, group) {
			var option = getOptionByGroup(group);

			var id = 'data-' + group + '-' + (angular.isNumber(object.id) ? object.id : object.asset.id);

			if (object.segment || (object.asset && object.asset.segment))
				id += '-' + typeService.segmentString(object).replace(',', '-');

			var newData = {
				parent: data,
				volume: volume,
				sessions: sessions,
				level: data.level + 1,

				id: id,
				object: object,
				permission: object.permission || volume.permission,
				group: group,
				items: [],

				select: false,
				expand: (focus && focus.id == group) ? ((angular.isDefined(focusInvert)) ? focusInvert : false) : option.expand
			};

			if (group == 'asset') {
				data.player = false;
				data.played = undefined;
			}

			browserService.groups[group].push(newData);

			if (group == 'session' && object.top)
				data.items.unshift(newData);
			else
				data.items.push(newData);

			return newData;
		};

		//

		browserService.isItemType = function (type) {
			return !!browserService.options[type];
		};

		browserService.isItemCategory = function (type) {
			return !!browserService.options.record.categories.find({id: type + ''});
		};

		//

		var recordGroupToggle = undefined;

		browserService.setRecordGroupToggle = function (group) {
			if (group == 'add') {
				var c = 0, maybe;

				angular.forEach(browserService.options.record.categories, function (recordGroup) {
					if (!recordGroup.active) {
						maybe = recordGroup;
						c++;
					}
				});

				if (c == 1) {
					browserService.addRecordGroup(maybe);
					return true;
				}
			}

			recordGroupToggle = angular.isUndefined(recordGroupToggle) ? group : undefined;
		};

		browserService.clearRecordGroupToggle = function () {
			if (angular.isDefined(recordGroupToggle))
				recordGroupToggle = undefined;
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

		browserService.addRecordGroup = function (group) {
			browserService.setRecordGroupToggle(undefined);

			var i = browserService.options.record.categories.index(group);

			group.active = true;

			browserService.options.record.categories.push(browserService.options.record.categories.splice(i, 1)[0]);

			browserService.rebuildData(group);
		};

		browserService.canRemoveRecordGroup = function () {
			return true;
		};

		browserService.removeRecordGroup = function (group) {
			group.active = false;

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

			browserService.rebuildData(maybe);
		};

		//

		browserService.setItemExpand = function (data, expand) {
			if (!data.expand && expand !== false) {
				data.expand = true;

				if (data.items == 0)
					browserService.updateData(data);
			} else if (data.expand && expand !== true) {
				data.expand = false;

				if (data == browserService.player)
					browserService.setItemPlayer(undefined);
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
				case 'asset':
					return browserService.options.asset;

				case 'session':
					return browserService.options.session;

				case 'volume':
					return browserService.options.volume;

				default:
					return browserService.options.record.categories.find({id: group});
			}
		};

		var getOptionByGroup = function (group) {
			switch (group) {
				case 'asset':
					return browserService.options.asset;

				case 'session':
					return browserService.options.session;

				case 'volume':
					return browserService.options.volume;

				default:
					return browserService.options.record.categories.find({id: group});
			}
		};

		browserService.getSorts = function (group) {
			return getOptionByGroup(group).sort;
		};

		var sortToggle = undefined;

		browserService.setSortToggle = function (sort) {
			sortToggle = angular.isUndefined(sortToggle) ? sort : undefined;
		};

		browserService.isSortToggle = function (sort) {
			return sortToggle == sort;
		};

		browserService.switchSort = function (group, sort, maybe) {
			browserService.setSortToggle(undefined);

			var option = getOptionByGroup(group);

			var sort_i = option.sort.index(sort),
				maybe_i = option.sort.index(maybe);

			if (sort.active != maybe.active) {
				sort.active = !sort.active;
				maybe.active = !maybe.active;
			}

			option.sort[sort_i] = option.sort.splice(maybe_i, 1, option.sort[sort_i])[0];

			browserService.filterDataGroup(getLevelByGroup(group));
		};

		browserService.canReverseSort = function () {
			return true;
		};

		browserService.reverseSort = function (group, sort) {
			sort.order = !sort.order;

			browserService.filterDataGroup(getLevelByGroup(group));
		};

		browserService.canRemoveSort = function () {
			return true;
		};

		browserService.removeSort = function (group, sort) {
			sort.active = false;

			var option = getOptionByGroup(group);

			// move to end
			var sort_i = option.sort.index(sort);

			option.sort.splice(sort_i, 1);
			option.sort.push(sort);

			browserService.filterDataGroup(getLevelByGroup(group));
		};

		browserService.canAddSort = function (group) {
			var canAdd = false;

			var option = getOptionByGroup(group);

			angular.forEach(option.sort, function (sort) {
				if (!canAdd && !sort.active)
					canAdd = true;
			});

			return canAdd;
		};

		browserService.addSort = function (group) {
			var go = true;

			var option = getOptionByGroup(group);

			angular.forEach(option.sort, function (sort) {
				if (go && !sort.active) {
					sort.active = true;
					go = false;
				}
			});

			browserService.filterDataGroup(getLevelByGroup(group));
		};

		var getLevelByGroup = function (group) {
			return getActiveGroups().indexOf(group);
		};

		//

		browserService.getObjectPermission = function (object) {
			var type = typeService.getType(object),
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

		browserService.hasAccess = function (data, level) {
			return data.permission >= level || authService.hasAuth('SUPER');
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

		browserService.player = undefined;

		browserService.setItemPlayer = function (data) {
			var newPlayer, newPlayed;

			if (angular.isUndefined(data)) {
				newPlayed = undefined;
				newPlayer = undefined;
			} else if (data.group == 'asset') {
				newPlayed = data;
				newPlayer = data.parent;
			} else {
				newPlayed = data.items[0] || undefined;
				newPlayer = data;
			}

			if (angular.isUndefined(browserService.player)) {
				browserService.player = newPlayer;

				browserService.player.player = true;

				browserService.player.played = newPlayed;
			} else if (browserService.player != newPlayer) {
				browserService.player.player = false;

				browserService.player = newPlayer;

				if (angular.isDefined(browserService.player)) {
					browserService.player.player = true;
					browserService.player.played = newPlayed;
				}
			} else if (browserService.player.played != newPlayed) {
				browserService.player.played = newPlayed;
			} else {
				browserService.player.player = false;

				browserService.player = undefined;
			}

			return browserService.player;
		};

		browserService.getItemPlayer = function () {
			return browserService.player;
		};

		browserService.isItemPlayer = function (data) {
			return data = browserService.player;
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
