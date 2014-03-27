define(['config/module'], function (module) {
	'use strict';

	module.controller('LoadingView', ['$scope', 'PageService', '$location', 'AuthService', function ($scope, page, $location, auth) {
		page.loading = true;

		if (auth.next) {
			$location.url(auth.next).replace();
			auth.next = undefined;
		}
	}]);
});
