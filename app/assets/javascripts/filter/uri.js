module.filter('uri', [function () {
	var encodeURIQuery = function (val, pctEncodeSpaces) {
		return encodeURIComponent(val).
			replace(/%40/gi, '@').
			replace(/%3A/gi, ':').
			replace(/%24/g, '$').
			replace(/%2C/gi, ',').
			replace(/%20/g, (pctEncodeSpaces ? '%20' : '+'));
	};

	return function (input, component, withSpaces) {
		if (component)
			return encodeURIQuery(input, withSpaces);

		return encodeURI(input);
	};
}]);
