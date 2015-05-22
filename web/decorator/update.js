'use strict';

app.factory('updateInterceptor', [
  '$rootScope', 'constantService', 'messageService', '$sce',
  function ($rootScope, constants, messages, $sce) {
    var version = '/' + constants.version;

    return {
      response: function (res) {
        var server;
        if (res.headers && (server = res.headers('server')) && !server.endsWith(version))
          messages.add({
            type: 'yellow',
            body: $sce.trustAsHtml(constants.message('app.update') + ' ' + constants.message('app.reload')),
            persist: true
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
