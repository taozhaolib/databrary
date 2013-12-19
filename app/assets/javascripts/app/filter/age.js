define(['app/config/module'], function (module) {
	'use strict';

	module.filter('age', ['$filter', function ($filter) {
		var daysTo = {
			months: 30.436849,
			years: 365.24219
		};

		return function (age, outputFormat, inputFormat) {
			if(!angular.isNumber(parseFloat(age)))
				return age;

			age = parseFloat(age);
			outputFormat = ['science'].indexOf(outputFormat) > -1 ? outputFormat : 'science';
			inputFormat = ['days'].indexOf(inputFormat) > -1 ? inputFormat : 'days';

			//

			var days;

			switch(inputFormat) {
				case 'seconds':
					days = age / 86400;
					break;

				case 'days':
				default:
					days = age;
					break;
			}

			//

			var output;

			switch(outputFormat) {
				case 'science':
				default:
					var months = days / daysTo.months;
					var years = days / daysTo.years;

					if(months < 3)
						output = Math.round(months * 10) / 10+' days';
					else if (months < 37)
						output = Math.round(months * 10) / 10+' months';
					else if (years >= 90)
						output = '90+ years';
					else
						output = Math.round(years * 10) / 10 + ' years';
					break;
			}

			return output;
		};
	}]);
});
