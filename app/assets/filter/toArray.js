'use strict';

module.filter('toArray', [
  'pageService', function (page) {
    return function (input, expression, reverse) {
      if (!angular.isObject(input)) {
	return [input];
      }

      var output = [];

      angular.forEach(input, function (item, key) {
	if (key != '$promise' && key != '$resolved') {
	  item.key = key;
	  output.push(item);
	}
      });

      if (angular.isDefined(expression)) {
	output = page.$filter('orderBy')(output, expression, reverse);
      }

      return output;
    };
  }
]);
