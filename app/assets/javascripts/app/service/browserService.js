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
			}
		};

		var DEFAULT_CATEGORY = {
			id: null,
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

		var NULL_CATEGORY = {
			id: 0,
			name: undefined
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

				case 'volume':
				case 'record':
				case 'session':

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
					if (!browserService.options.record.categories.get({
						id: category
					}))
						browserService.options.record.categories.push(angular.extend({}, DEFAULT_CATEGORY, {
							id: category,
							name: $rootScope.constant.get('category', category).name
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
			var groups = getActiveRecordGroups();

			var data = angular.extend({}, DEFAULT_DATA);

			if (groups.length == 0)
				return;

			data = updateData(data, groups);

			browserService.data = data;
		};

		var getActiveRecordGroups = function () {
			var groups = getActiveGroups();

			if (groups[0] == 'volume')
				groups.unshift();

			if (groups[groups.length - 1] == 'session')
				groups.pop();

			return groups;
		};

		var getActiveGroups = function () {
			var groups = [];

			if (isGroupActive('volume'))
				groups.push('volume');

			if (browserService.options.record)
				angular.forEach(browserService.options.record.categories, function (category) {
					if (category.allow && category.active)
						groups.push(category.id);
				});

			if (isGroupActive('session'))
				groups.push('session');

			return groups;
		};

		var isGroupActive = function (group) {
			return browserService.options[group].allow && browserService.options[group].active;
		};

		var updateData = function (data, groups) {
			// note: always sort volumes
			angular.forEach(raw, function (volume, volumeID) {
				var newData = angular.extend({}, DEFAULT_GROUP, {
					object: volume
				});

				data.items.push(newData);

				// start the record-session mayhem
				updateRecordsCallback(newData, volume, volume.sessions, groups, 1);
			});

			return data;
		};

		var updateRecordsCallback = function (data, volume, sessions, groups, level) {
			var categoryID = groups[++level],
				recordLevel = categoryID != 'session'; // important for the callback at the end

			if (angular.isUndefined(categoryID) || categoryID != 'session')
				updateSessionsCallback(data, volume, sessions, groups, --level);

			var tempData = {};

			// iterate through sessions
			angular.forEach(sessions, function (session, sessionID) {
				var categoryRecords = session.categories[$rootScope.constant.get('category', categoryID)] || {};

				if(!$.isEmptyObject(categoryRecords)) {
					// iterate through records
					angular.forEach(categoryRecords, function (record, recordID) {
						// make level's record -> sessions
						if(tempData[recordID])
							tempData[recordID].push(session);
						else
							tempData[recordID] = [session];
					});

					if(!$.isEmptyObject(tempData)) {
						angular.forEach(tempData, function (newSessions, recordID) {
							//
							// DUPLICATE OF updateData
							//
							var newData = angular.extend({}, DEFAULT_GROUP, {
								object: volume.records[recordID]
							});

							data.items.push(newData);

							// continue record-session mayhem
							updateRecordsCallback(newData, volume, newSessions, groups, level);
						});
					}
				}
			});
		};

		var updateSessionsCallback = function (data, volume, sessions, groups, level) {
			angular.forEach(sessions, function (session, sessionID) {
				data.items.push(session);
			});
		};

//		var updateRecordCallback = function (data, volume, sessions, groups, level) {
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

		return browserService;
	}]);
});
