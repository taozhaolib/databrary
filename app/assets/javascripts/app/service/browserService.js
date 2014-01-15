define(['app/config/module'], function (module) {
	'use strict';

	module.factory('BrowserService' ['$rootScope', 'ArrayHelper', function ($rootScope, arrayHelper) {
		var browserService = {};

		//

		browserService.data = [];
		browserService.recordSorts = arrayHelper([]);

		browserService.sort = undefined;

		browserService.setData = function (data) {
			browserService.data = data;

			$rootScope.$watch('browser.data', function () {
				var tempSorts = arrayHelper([]);

				angular.forEach(browserService.data, function (volume) {
					angular.forEach(volume.records, function (record) {
						var temp_i = tempSorts.index({name: record.category}),
							real_i = browserService.recordSorts.index({name: record.category});

						if(temp_i == -1)
							if(real_i > -1)
								tempSorts.update(temp_i, browserService.recordSorts.get(real_i));
							else
								tempSorts.add({
									name: record.category,
									active: false
								});
					});
				});

				if(browserService.recordSorts.length == 0 && tempSorts.index({name: 'participant'}))
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
			if(contexts.indexOf(newContext) == -1)
				return false;

			switch(newContext) {
				case 'search':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: true,
							session: true,
							asset: true,
							record: ['participant']
						};
					break;

				case 'party':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: true,
							session: true,
							asset: true,
							record: ['participant']
						};
					break;

				case 'volume':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: false,
							session: true,
							asset: true,
							record: ['participant']
						};
					break;

				case 'record':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = {
							volume: false,
							session: true,
							asset: true,
							record: ['participant']
						};
					break;

				case 'session':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = { // questioned
							volume: false,
							session: true,
							asset: true,
							record: undefined
						};
					break;

				case 'asset':
					if(angular.isUndefined(browserService.sort))
						browserService.sort = { // questioned
							volume: false,
							session: true,
							asset: true,
							record: undefined
						};
					break;
			}

			return newContext;
		};

		//

		return browserService;
	}]);
});
