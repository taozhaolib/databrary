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
      data = {action: action, route: location.pathname, data: data};
      if (!_.some(queue, data))
        queue.push(data);
    };

    return analytics;
  }
]);
