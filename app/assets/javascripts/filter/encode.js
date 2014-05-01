module.filter('encode', [
	function () {
		return function (input, component) {
			return component ? encodeURIComponent(input) : encodeURI(input);
		};
	}
]);
