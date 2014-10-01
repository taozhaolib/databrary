'use strict';

module.directive('uploadProgress', [
  function () {

    return {
      restrict: 'E',
      scope: {
        progressFloat: '=progressValue'
      },
      templateUrl: 'asset/uploadProgress.html',
      };
  }
]);

