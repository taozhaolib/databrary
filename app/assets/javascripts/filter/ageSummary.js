define(['config/module'], function (module) {
	'use strict';

	module.filter('ageSummary', ['$filter', 'ConstantService', function ($filter, constantService) {
		return function (summary) {
			var age = $filter('age');
			var range = age(summary.agerange[0]);
			if (summary.agerange[0] != summary.agerange[1])
				range += ' - ' + age(summary.agerange[1]);
			return constantService.message('volume.ages', range, age(summary.agemean));
		};
	}]);
});
