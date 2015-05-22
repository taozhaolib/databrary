'use strict';

app.filter('age', [
  function () {
    var YEAR = 365.24219;
    var MONTH = YEAR/12;
    var LIMIT = 90*YEAR;

    return function (days, outputFormat) {
      if (!angular.isNumber(days)) {
        return days;
      }

      if (days >= LIMIT)
        return "90+ yrs";

      switch (outputFormat) {
        case 'years':
          return (days/YEAR).toFixed(1) + ' yrs';

        case 'months':
          return (days/MONTH).toFixed(1) + ' mos';

        case 'days':
          return days.toFixed() + ' days';

        default:
          var months = days / MONTH;
          if (months < 3)
            return days.toFixed() + ' days';
          else if (months < 37)
            return months.toFixed(1) + ' mos';
          else
            return (days/YEAR).toFixed(1) + ' yrs';
      }
    };
  }
]);
