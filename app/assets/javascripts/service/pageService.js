module.factory('pageService', [
	'$rootScope',
	'messageService',
	'tooltipService',
	'constantService',
	'routerService',
	'eventService',
	function ($rootScope, messages, tooltips, constants, router, events) {
		var pageService = {
			messages: messages,
			tooltips: tooltips,
			constants: constants,
			router: router,
			events: events
		};

		//

		pageService.title = 'Welcome!';

		//

		pageService.loading = false;

		//

		$rootScope.$on('$routeChangeStart', function () {
			pageService.loading = true;
		});

		$rootScope.$on('$routeChangeSuccess', function () {
			pageService.loading = false;
		});

		//

		return pageService;
	}
]);
