define(['config/module'], function (module) {
	'use strict';

	module.factory('PageService', ['$rootScope', function ($rootScope) {
		var pageService = {};

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
