define(['config/module'], function (module) {
	'use strict';

	var ent = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;" };

	module.filter('format', [function () {
		return function (input, lineMode, raw) {
			if(!angular.isString(input))
				return '';

			if (!raw)
				/* TODO: please is there a better way to do this? */
				input = input.replace(/[&<>"']/g, function(c) { return ent[c]; });

			if (lineMode)
				return input.replace(/: /g, ': <br>');

			return '<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
		};
	}]);
});
