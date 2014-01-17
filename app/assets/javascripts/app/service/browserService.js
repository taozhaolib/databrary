define(['app/config/module'], function (module) {
	'use strict';

	module.factory('BrowserService', ['$rootScope', 'ArrayHelper', function ($rootScope, arrayHelper) {
		var browserService = {};

		//

		browserService.data = [];
		browserService.recordSorts = arrayHelper([]);

		browserService.sort = undefined;

		browserService.setData = function (data) {
			browserService.data = data;

			$rootScope.$watch('browser.data', function () {
				var tempSorts = arrayHelper([]);

				// inefficient.
				angular.forEach(browserService.data, function (volume) {
					angular.forEach(volume.records, function (record) {
						var temp_i = tempSorts.index({name: record.category});

						if (temp_i == -1) {
							var real_i = browserService.recordSorts.index({name: record.category});

							if (real_i > -1)
								tempSorts.add(browserService.recordSorts.get(real_i));
							else
								tempSorts.add({
									name: record.category,
									active: false,
									enabled: true
								});
						}
					});
				});

				if (browserService.recordSorts.length == 0 && tempSorts.index({name: 'participant'}) > -1)
					tempSorts.update({name: 'participant'}, {active: true});

				browserService.recordSorts = tempSorts;
			}, true);
		};

		//

		var contexts = ['search', 'party', 'volume', 'record', 'session', 'asset'];
		var context = undefined;

		browserService.getContext = function () {
			return context;
		};

		browserService.initialize = function (newContext) {
			if (contexts.indexOf(newContext) == -1)
				return false;

			switch (newContext) {
				case 'search':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: true,
								allow: true
							},
							record: { // TEMP!
								active: true,
								allow: false
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;

				case 'party':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: true,
								allow: true
							},
							record: {
								active: true,
								allow: true
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;

				case 'volume':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: false,
								allow: false
							},
							record: {
								active: true,
								allow: true
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;

				case 'record':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: false,
								allow: false
							},
							record: {
								active: true,
								allow: true
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;

				case 'session':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: false,
								allow: false
							},
							record: {
								active: true,
								allow: true
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;

				case 'asset':
					if (angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: {
								active: false,
								allow: false
							},
							record: {
								active: false,
								allow: false
							},
							session: {
								active: true,
								allow: true
							},
							asset: {
								active: true,
								allow: true
							}
						};
					break;
			}

			context = newContext;

			return newContext;
		};

		//

		browserService.getSortLevels = function ()  {
			var levels = [];

			if(browserService.sort.volume.active)
				levels.push('volume');

			if(browserService.sort.record.allow) {
				angular.forEach(browserService.recordSorts, function (sort) {
					if(sort.active)
						levels.push(sort.name);
				});
			}

			if(browserService.sort.session.active)
				levels.push('session');

			if(browserService.sort.asset.active)
				levels.push('asset');

			return levels;
		};

		browserService.getLevelType = function (depth) {
			if(!angular.isNumber(depth))
				return undefined;

			return browserService.getSortLevels()[depth];
		};

		browserService.getLevelItems = function (depth, args) {
			args = angular.extend({}, args);

			var type = browserService.getLevelType(depth);

			switch(type) {
				case 'volume':
					return browserService.data;

				case 'session':
					switch(args.parent) {
						case 'volume':
							var tmp = [];

							angular.forEach(browserService.data, function (volume) {
								if(volume.id = args.volume)
									tmp = volume.sessions
							});

							return tmp;

						default:
							return [];
					}

				case 'asset':
					switch(args.parent) {
						case 'volume':
							var tmp = [];

							angular.forEach(browserService.data, function (volume) {
								if(volume.id = args.volume)
									tmp = volume.assets
							});

							return tmp;

						case 'session':
							var tmp = [];

//							angular.forEach(browserService.data, function (volume) {
//								if(volume.id = args.volume)
//									angular.forEach(volume.assets, function(asset) {console.log(asset.container.id);
//										if(asset.container.id = args.session)
//											tmp.push(asset);
//									});
//							});

							return tmp;

						default:
							return [];
					}

				default:
					return [];
			}
		};

		browserService.hasLevelItems = function (depth, args) {
			return browserService.getLevelItems(depth, args).length > 0;
		};

		//

		return browserService;
	}]);
});
