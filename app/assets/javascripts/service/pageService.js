module.factory('pageService', [
	'$injector', function ($injector) {
		var page = {
			analytics: $injector.get('analyticService'),
			auth: $injector.get('authService'),
			authPresets: $injector.get('authPresetService'),
			browser: $injector.get('browserService'),
			constants: $injector.get('constantService'),
			display: $injector.get('displayService'),
			events: $injector.get('eventService'),
			gui: $injector.get('guiService'),
			messages: $injector.get('messageService'),
			models: $injector.get('modelService'),
			panels: $injector.get('panelService'),
			router: $injector.get('routerService'),
			tooltips: $injector.get('tooltipService'),
			types: $injector.get('typeService'),
		};

		//

		angular.forEach([
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
		], function (dependency) {
			page[dependency] = $injector.get(dependency);
		});

		//

		page.$b = $('body');
		page.$m = $('main');
		page.$d = $(page.$document);
		page.$w = $(page.$window);

		//

		return page;
	}
]);
