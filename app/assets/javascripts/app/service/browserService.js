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
				categories: {}
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
				active: false,
				expand: true,
				filter: {},
				order: []
			}
		};

		var DEFAULT_CATEGORY = {
			allow: true,
			active: true,
			expand: true
		};

		//

		var data = {};

		var contexts = ['search', 'party', 'volume', 'record', 'session', 'asset'];
		var context = undefined;

		//

		browserService.data = {
//			expand: true,
//			items: [
//				{
//					item: null,
//					expand: true,
//					items: [
//						{
//							item: null,
//							expand: true,
//							items: [
//								{
//									item: null,
//									expand: true,
//									items: [
//										{
//											item: null,
//											expand: true,
//											items: [
//												{
//													item: null,
//													expand: true
//												}
//											]
//										}
//									]
//								}
//							]
//						}
//					]
//				}
//			]
		};

		browserService.options = {};

		//

		browserService.initialize = function (newContext, newData) {
			browserService.initializeData(newData);
			browserService.initializeOptions(newContext);
		};

		//

		browserService.initializeOptions = function (newContext) {
			if (contexts.indexOf(newContext) == -1)
				return false;

			context = newContext;

			angular.extend(browserService.options, DEFAULT_OPTIONS);

			browserService.updateCategories();

			switch (context) {
				case 'search':
					browserService.options.volume.allow = true;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;

				case 'volume':
					browserService.options.volume.allow = false;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;

				case 'record':
					browserService.options.volume.allow = false;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;

				case 'session':
					browserService.options.volume.allow = false;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;

				case 'asset':
					browserService.options.volume.allow = false;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;

				case 'party':
					browserService.options.volume.allow = true;
					browserService.options.record.allow = true;
					browserService.options.session.allow = true;
					browserService.options.asset.allow = true;

					if (angular.isObject(browserService.options.record.categories['participant']))
						browserService.options.record.categories['participant'].active = true;

					break;
			}

			return context;
		};

		browserService.updateCategories = function () {
			angular.forEach(data, function (volume) {
				angular.forEach(volume.categories, function (sessions, category) {
					if (!angular.isArray(browserService.options.record.categories[category]))
						browserService.options.record.categories[category] = angular.copy(DEFAULT_CATEGORY);
				});
			});
		};

		browserService.getContext = function () {
			return context;
		};

		//

		browserService.initializeData = function (newData) {
			data = {};

			if (newData.id)
				data[newData.id] = newData;
			else if (angular.isObject(newData))
				data = newData;

			browserService.updateData();
		};

		browserService.updateData = function () {
			// convert .data into functional hierarchy
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

			angular.forEach(data, function (volume) {
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
