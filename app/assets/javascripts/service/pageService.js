module.factory('pageService', [
	'$rootScope',
	'messageService',
	'tooltipService',
	'constantService',
	'routerService',
	'eventService',
	'guiService',
	'modelService',
	'authService',
	function ($rootScope, messages, tooltips, constants, router, events, gui, models, auth) {
		var page = {
			messages: messages,
			tooltips: tooltips,
			constants: constants,
			router: router,
			gui: gui,
			events: events,
			models: models,
			auth: auth
		};

		//

		page.title = 'Welcome!';

		//

		page.loading = false;

		$rootScope.$on('$routeChangeStart', function () {
			page.loading = true;
		});

		$rootScope.$on('$routeChangeSuccess', function () {
			page.loading = false;
		});

		//

		page.$window = $(window);
		page.$document = $(document);

		//

		return page;
	}
]);
