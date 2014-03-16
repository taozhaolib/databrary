define(['config/module'], function (module) {
	'use strict';

	module.filter('truncate', ['$filter', function ($filter) {
		return function (text, length, type, end) {
			length = angular.isNumber(parseInt(length)) ? parseInt(length) : 10;
			type = ['characters', 'words'].indexOf(type) ? type : 'characters';
			end = angular.isDefined(end) ? end : "...";

			switch (type) {
				case 'words':
					var words = text.split(' ');

					if (words.length < length)
						return text;
					else
						return words.splice(0, length).join(' ') + end;

				case 'characters':
				default:
					if (text.length <= length || text.length - end.length <= length)
						return text;
					else
						return String(text).substring(0, length - end.length) + end;
			}


		};
	}]);
});
