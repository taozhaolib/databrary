/*!
 * jQuery plugins
 */
(function ($) {
	$.fn.exists = function () {
		return this.length !== 0;
	};

	$.fn.escape = function () {
		return this
			.replace(/&/g, "&amp;")
			.replace(/</g, "&lt;")
			.replace(/>/g, "&gt;")
			.replace(/"/g, "&quot;")
			.replace(/'/g, "&#039;");
	};

	$.fn.unescape = function () {
		return this
			.replace(/&amp;/g, "&")
			.replace(/&lt;/g, "<")
			.replace(/&gt;/g, ">")
			.replace(/&quot;/g, "\"")
			.replace(/&#039;/g, "'");
	};
})(jQuery);
