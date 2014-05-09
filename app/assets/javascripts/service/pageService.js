module.factory('pageService', [
	'$rootScope',
	'messageService',
	'tooltipService',
	'constantService',
	'typeService',
	'routerService',
	'eventService',
	'guiService',
	'modelService',
	'authService',
	'analyticService',
	'$location',
	'$filter',
	'$timeout',
	function ($rootScope, messages, tooltips, constants, types, router, events, gui, models, auth, analytics, $location, $filter, $timeout) {
		var page = {
			messages: messages,
			tooltips: tooltips,
			constants: constants,
			types: types,
			router: router,
			gui: gui,
			events: events,
			models: models,
			auth: auth,
			analytics: analytics,
			$location: $location,
			$filter: $filter,
			$timeout: $timeout,
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
				body: constants.message('app.update') + ' <a href="" onclick="window.location.reload()">Reload</a>.'
			});
		});

		//

		return page;
	}
]);
