define(['app/config/module'], function (module) {
	'use strict';

	module.filter('format', [function () {
		return function (input, mode) {
			if (mode == 'title')
				return input.replace(/: /g, ': <br>');

			return '<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
		};
	}]);
});
