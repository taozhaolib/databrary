'use strict';

app.factory('$exceptionHandler', [
  '$log', 'constantService',
  function ($log, constants) {
    return constants.mode === 'Dev' ?
      function (exception, cause) {
        if (cause)
          exception.message += ' (caused by "' + cause + '")';
        $log.error.apply($log, arguments);
        throw exception;
      } : function () {
        $log.error.apply($log, arguments);
      };
  }
]);
