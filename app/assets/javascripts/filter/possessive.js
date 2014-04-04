define(['config/module'], function (module) {
	'use strict';

	module.filter('possessive', ['$rootScope', 'pageService', function ($rootScope, page) {
		return function (key, party, name) {
			var replace;

			if($rootScope.auth.user.id == party.id)
				replace = 'my';
			else
				replace = (name ? name : party.name)+"'s";

			return page.constants.message(key, replace);
		};
	}]);
});
