'use strict';

app.filter('timecode', [
  function () {
    var pad = function (s, n, d) {
      var p;
      if (d === undefined || (p = s.indexOf(d)) === -1)
        p = s.length;
      while (p++ < n)
        s = '0' + s;
      return s;
    };

    return function (input, showMilli) {
      if (!angular.isNumber(input)) {
        return input;
      }

      var i = Math.abs(input);
      var s = (i%60000)/1000;
      if (!showMilli)
        s |= 0;
      i = 0|i/60000;
      s = (i%60) + ':' + pad(showMilli ? s.toFixed(3) : (s|0).toString(), 2, '.');
      i = 0|i/60;
      if (i) {
        s = (i%24) + ':' + pad(s, 2, ':');
        i = 0|i/24;
        if (i)
          s = i + ':' + pad(s, 2, ':');
      }
      if (input < 0)
        s = '-' + s;

      return s;
    };
  }
]);
