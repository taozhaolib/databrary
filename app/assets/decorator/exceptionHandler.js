'use strict';

if (DEBUG) app.factory('$exceptionHandler', [
  '$log',
  function ($log) {
    return function (exception, cause) {
      $log.error.apply($log, arguments);
      throw exception;
    };
  }
]);
