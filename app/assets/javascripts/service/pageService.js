module.factory('pageService', [
	'$rootScope',
	'messageService',
	'tooltipService',
	'constantService',
	'routerService',
	'eventService',
	'guiService',
	function ($rootScope, messages, tooltips, constants, router, events, gui) {
		var page = {
			messages: messages,
			tooltips: tooltips,
			constants: constants,
			router: router,
			gui: gui,
			events: events
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

		events.listen($rootScope, 'pageService-updateApp', function () {
			messages.add({
				type: 'yellow',
				body: constants.message('app.update')
			});
		});

		//

		return page;
	}
]);
