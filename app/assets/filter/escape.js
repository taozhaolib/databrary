'use strict';

module.filter('escape', [
  '$sce', '$sanitize',
  function ($sce, $sanitize) {
    return function (input) {
      return $sce.trustAsHtml($sanitize(input));
    };
  }
]);
