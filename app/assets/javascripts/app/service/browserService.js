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

		browserService.setContext = function (newContext) {
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

		return browserService;
	}]);
});
