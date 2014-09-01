'use strict';

module.filter('format', [
  '$sce', '$sanitize',
  function ($sce, $sanitize) {
    return function (input, lineMode) {
      if (input === undefined || input === null)
	return '';

      input = $sanitize(input);
      return $sce.trustAsHtml(lineMode ? input :
	'<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>');
    };
  }
]);
