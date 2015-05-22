'use strict';

app.filter('handle', [
  function () {
    var urlTypes = {
      doi: {
        prefix: "doi:",
        handler: "http://dx.doi.org/"
      },

      hdl: {
        prefix: "hdl:",
        handler: "http://hdl.handle.org/"
      }
    };

    return function (origUrl) {
      for (var i in urlTypes) {
        if (origUrl && origUrl.startsWith(urlTypes[i].prefix)) {
          return urlTypes[i].handler + origUrl.substr(urlTypes[i].prefix.length);
        }
      }

      return origUrl;
    };
  }
]);
