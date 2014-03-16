define(['config/module'], function (module) {
	'use strict';

	module.filter('gravatar', ['$filter', function ($filter) {
		return function (party, size) {
			if (!angular.isNumber(size))
				size = 512;

			if (/^([a-zA-Z0-9_.+-])+\@(([a-zA-Z0-9-])+\.)+([a-zA-Z0-9]{2,4})+$/.test(party))
				return 'http://gravatar.com/avatar/' + $filter('md5')(party) + '?s=' + size + '&d=mm';

			if (angular.isString(party))
				return party;

			return '/public/images/profiles/unknown.png';
		};
	}]);
});
