define(['config/module'], function (module) {
	'use strict';

	module.filter('possessive', ['$rootScope', 'ConstantService', function ($rootScope, constantService) {
		return function (key, party, name) {
			var replace;

			if($rootScope.auth.user.id == party.id)
				replace = 'My';
			else
				replace = (name ? name : party.name)+"'s";

			return constantService.message(key, replace);
		};
	}]);
});
