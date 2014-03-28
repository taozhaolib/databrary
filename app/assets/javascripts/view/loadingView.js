define(['config/module'], function (module) {
	'use strict';

	module.controller('LoadingView', ['$scope', 'PageService', '$location', 'AuthService', 'ConstantService', function ($scope, page, $location, auth, constants) {
		page.loading = true;
		page.title = constants.message('page.title.loading');

		if (auth.next) {
			$location.url(auth.next).replace();
			auth.next = undefined;
		}
	}]);
});
