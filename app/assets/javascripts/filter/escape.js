var ent = {
	"&": "&amp;",
	"<": "&lt;",
	">": "&gt;",
	'"': "&quot;",
	"'": "&apos;"
};

module.filter('escape', [
	function () {
		return function (input) {
			if (!angular.isString(input))
				return '';

			return input.replace(/[&<>"']/g, function (c) {
				return ent[c];
			});
		};
	}
]);
