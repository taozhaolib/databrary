'use strict';

app.factory('analyticService', [
  function () {
    var analytics = {};
    var queue = [];

    analytics.dump = function () {
      if (!queue.length)
        return;
      return JSON.stringify(queue.splice(0, queue.length));
    };

    analytics.add = function (action, data) {
      queue.push({action: action, route: location.pathname, data: data});
    };

    return analytics;
  }
]);
