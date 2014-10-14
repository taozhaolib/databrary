'use strict';

app.factory('$exceptionHandler', [
  '$log', 'constantService',
  function ($log, constants) {
    return constants.mode === 'Dev' ?
      function (exception, cause) {
        exception.message += ' (caused by "' + cause + '")';
        throw exception;
      } : function () {
        $log.error.apply($log, arguments);
      };
  }
]);
