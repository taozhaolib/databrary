'use strict';

module.filter('uri', [
  function () {
    var encodeURIQuery = function (val, pctEncodeSpaces) {
      var e = encodeURIComponent(val).
        replace(/%40/g, '@').
        replace(/%3A/gi, ':').
        replace(/%24/g, '$').
        replace(/%2C/gi, ',');
      if (!pctEncodeSpaces)
        e = e.replace(/%20/g, '+');
      return e;
    };

    return function (input, component, withSpaces) {
      if (component) {
        return encodeURIQuery(input, withSpaces);
      }

      return encodeURI(input);
    };
  }
]);
