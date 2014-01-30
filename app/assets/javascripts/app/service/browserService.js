define(['app/config/module'], function (module) {
	'use strict';

	module.factory('BrowserService', ['$rootScope', 'ArrayHelper', function ($rootScope, arrayHelper) {
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
				active: false,
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
			active: false,
			expand: false,
			expanded: null // array
		};

		//

		var raw = {};

		var contexts = ['search', 'party', 'volume', 'record', 'session'];
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
				case 'party':
				case 'search':
					browserService.options.volume.allow = true;

					break;

				case 'volume':
//				case 'record':
//				case 'session':
					var participant = browserService.options.record.categories.get({name: 'participant'});

					if (participant)
						participant.active = true;

					break;
			}

			return context;
		};

		browserService.updateCategories = function () {
			angular.forEach(raw, function (volume) {
				angular.forEach(volume.categories, function (sessions, category) {
					if (!browserService.options.record.categories.get({
						id: category
					}))
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

		browserService.updateData = function () {
			var start = new Date().getTime();

			var groups = getActiveGroups();

			var data = {
				items: []
			};

			data = updateData(data, groups);

			browserService.data = data;

			var end = new Date().getTime();
			console.log(end - start);
		};

		var getActiveRecordGroups = function () {
			var groups = [];

			angular.forEach(getActiveGroups(), function (group) {
				var group = parseInt(group);

				if (!isNaN(group))
					groups.push(group);
			});

			return groups;
		};

		var getActiveGroups = function () {
			var groups = [];

			if (isGroupActive('volume'))
				groups.push('volume');

			if (browserService.options.record)
				angular.forEach(browserService.options.record.categories, function (category) {
					if (category.allow && category.active)
						groups.push(parseInt(category.id));
				});

			if (isGroupActive('session'))
				groups.push('session');

			return groups;
		};

		var isGroupActive = function (group) {
			return browserService.options[group].allow && browserService.options[group].active;
		};

		var updateData = function (data, groups) {
			if (!groups[0])
				return data;

			switch (groups[0]) {
				case 'volume':
					updateVolumesCallback(data, groups);
					break;

				case 'session':
					angular.forEach(raw, function (volume, volumeID) {
						updateSessionsCallback(data, volume, volume.sessions);
					});
					break;

				default:
					angular.forEach(raw, function (volume, volumeID) {
						updateRecordsCallback(data, volume, volume.sessions, groups, 0);
					});
					break;
			}

			return data;
		};

		var updateVolumesCallback = function (data, groups) {
			angular.forEach(raw, function (volume, volumeID) {
				var newData = updateItemCallback(data, volume, 'volume');

				updateRecordsCallback(newData, volume, volume.sessions, groups, 1);
			});

			return data;
		};

		var updateRecordsCallback = function (data, volume, sessions, groups, level) {
			var categoryID = groups[level];

			if (categoryID == 'session')
				return updateSessionsCallback(data, volume, sessions);

			var tempData = {};

			angular.forEach(sessions, function (session, sessionID) {
				var categoryRecords = session.categories[categoryID];

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
					var newData = updateItemCallback(data, volume.records[recordID], 'record');

					updateRecordsCallback(newData, volume, newSessions, groups, level + 1);
				});
			}

			return data;
		};

		var updateSessionsCallback = function (data, volume, sessions) {
			angular.forEach(sessions, function (session, sessionID) {
				updateItemCallback(data, session, 'session');
			});

			return data;
		};

		var updateItemCallback = function (data, object, type) {
			var newData = {
				object: object,
				type: type,
				items: []
			};

			data.items.push(newData);

			return newData;
		};

		//

		var types = ['volume', 'record', 'session'];

		browserService.setItemExpand = function (object, expand, type) {
			var option;

			type = types.indexOf(type) > -1 ? type : browserService.getItemType(object);

			switch (type) {
				case 'volume':
					option = browserService.options.volume;
					break;

				case 'session':
					option = browserService.options.session;
					break;

				case 'record':
					option = browserService.options.record.categories[object.category];
					break;

				default:
					return undefined;
					break;
			}

			var index = option.expanded.indexOf(object.id);

			if (index == -1 && expand !== false)
				option.expanded.push(object.id);
			else if (index > -1 && expand !== true)
				option.expanded.splice(index, 1);

			return true;
		};

		browserService.getItemExpand = function (object, type) {
			var option;

			type = types.indexOf(type) > -1 ? type : browserService.getItemType(object);

			switch (type) {
				case 'volume':
					option = browserService.options.volume;
					break;

				case 'session':
					option = browserService.options.session;
					break;

				case 'record':
					option = browserService.options.record.categories[object.category];
					break;

				default:
					return undefined;
					break;
			}

			return option.expanded.indexOf(object.id) > -1;
		};

		browserService.getItemType = function (object) {
			if (!angular.isObject(object))
				return undefined;

			if (object.measures)
				return 'record';

			if (object.body)
				return 'volume';

			return 'session';
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
