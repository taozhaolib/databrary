'use strict';

module.filter('format', [
  'pageService', function (page) {
    var esc = page.$filter('escape');
    return function (input, lineMode, raw) {
      if (input === undefined || input === null)
	return '';

      if (!raw)
        input = esc(input);

      if (lineMode)
        return input;
      else
	return '<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
    };
  }
]);
