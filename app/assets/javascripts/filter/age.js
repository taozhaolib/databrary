define(['config/module'], function (module) {
	'use strict';

	module.filter('age', [function () {
		var daysTo = {
			months: 30.436849,
			years: 365.24219
		};

		return function (age, outputFormat, inputFormat, decimals) {
			if (!angular.isNumber(parseFloat(age)))
				return age;

			var formats = ['seconds', 'days', 'months', 'years', 'science'];

			age = parseFloat(age);
			outputFormat = formats.indexOf(outputFormat) > -1 ? outputFormat : 'science';
			inputFormat = ['days', 'months'].indexOf(inputFormat) > -1 ? inputFormat : 'days';
			decimals = $.isNumeric(decimals) ? parseInt(decimals) : 1;

			//

			var fix = function (value) {
				return new Number(value+'').toFixed(decimals);
			};

			//

			var days, seconds;

			switch (inputFormat) {
				case 'seconds':
					seconds = age;
					days = age / 86400;
					break;

				case 'days':
				default:
					seconds = age * 86400;
					days = age;
					break;
			}

			var months = days / daysTo.months;
			var years = days / daysTo.years;

			//

			var output;

			switch (outputFormat) {
				case 'years':
					output = fix(years) + ' years';
					break;

				case 'months':
					output = fix(months) + ' months';
					break;

				case 'days':
					output = fix(days) + ' days';
					break;

				case 'seconds':
					output = fix(seconds) + ' days';
					break;

				case 'science':
				default:
					if (months < 3)
						output = fix(days) + ' days';
					else if (months < 37)
						output = fix(months) + ' months';
					else if (years >= 90)
						output = '90+ years';
					else
						output = fix(years) + ' years';
					break;
			}

			return output;
		};
	}]);
});
