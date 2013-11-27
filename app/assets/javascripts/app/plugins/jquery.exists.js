define(['jquery'], function ($) {
	'use strict';

	$.fn.exists = function () {
		return this.length !== 0;
	};

	return $;
});
