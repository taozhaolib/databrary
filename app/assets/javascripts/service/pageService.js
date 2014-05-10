module.factory('pageService', [
	'$injector', function ($injector) {
		var dependencies = [
			'$anchorScroll',
			'$animate',
			'$cacheFactory',
			'$compile',
			'$document',
			'$filter',
			'$http',
			'$injector',
			'$interpolate',
			'$interval',
			'$location',
			'$log',
			'$parse',
			'$provide',
			'$q',
			'$rootScope',
			'$route',
			'$routeParams',
			'$sanitize',
			'$sce',
			'$templateCache',
			'$timeout',
			'$window',

			'$localStorage',
			'$sessionStorage',

			'analyticService',
			'authService',
			'constantService',
			'displayService',
			'eventService',
			'guiService',
			'messageService',
			'modelService',
			'panelService',
			'routerService',
			'tooltipService',
			'typeService',
		];

		var page = {};

		angular.forEach(dependencies, function (dependency) {
			page[dependency] = $injector.get(dependency);
		});

		page.$b = $('body');
		page.$m = $('main');
		page.$d = $(page.$document);
		page.$w = $(page.$window);

		return page;
	}
]);
