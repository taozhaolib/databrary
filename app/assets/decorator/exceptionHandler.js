'use strict';

if (DEBUG) app.factory('$exceptionHandler', [
  '$log',
  function ($log) {
    return function (exception, cause) {
      if (cause)
        exception.message += ' (caused by "' + cause + '")';
      $log.error.apply($log, arguments);
      throw exception;
    };
  }
]);
