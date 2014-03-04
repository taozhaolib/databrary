define(['app/config/module'], function (module) {
	'use strict';

	module.filter('possessive', ['$rootScope', '$filter', function ($rootScope, $filter) {
		return function (string, party, name) {
			var replace;

			if($rootScope.auth.user.id == party.id)
				replace = 'my';
			else
				replace = (name ? name : party.name)+"'s";

			return string.replace('{0}', replace);
		};
	}]);
});
