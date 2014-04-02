define(['config/module'], function (module) {
	'use strict';

	module.factory('Page', ['$rootScope', 'MessageService', 'TooltipService', 'ConstantService', 'RouterService', function ($rootScope, messages, tooltips, constants, router) {
		var pageService = {
			messages: messages,
			tooltips: tooltips,
			constants: constants,
			router: router
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
	}]);
});
