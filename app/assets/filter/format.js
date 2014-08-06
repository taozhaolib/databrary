'use strict';

module.filter('format', [
  'pageService', function (page) {
    return function (input, lineMode, raw) {
      if (!angular.isString(input)) {
        return '';
      }

      if (!raw) {
        input = page.$filter('escape')(input);
      }

      if (lineMode) {
        return input;
      }

      return '<p>' + input.replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
    };
  }
]);
