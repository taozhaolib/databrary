'use strict';

app.factory('updateInterceptor', [
  '$rootScope', 'constantService', 'messageService', '$sce',
  function ($rootScope, constants, messages, $sce) {
    var version = '/' + constants.version;
    var warning = false;

    return {
      response: function (res) {
        var server;
        if (!warning && res.headers && (server = res.headers('server')) && !server.endsWith(version))
          warning = messages.add({
            type: 'yellow',
            body: $sce.trustAsHtml(constants.message('app.update') + ' ' + constants.message('app.reload'))
          });

        return res;
      }
    };
  }
]);

app.config([
  '$httpProvider', function ($httpProvider) {
    $httpProvider.interceptors.push('updateInterceptor');
  }
]);
