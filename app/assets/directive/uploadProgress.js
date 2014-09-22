'use strict';

module.directive('uploadProgress', [
  function () {

    return {
      restrict: 'E',
      scope: {
	progressFloat: '=progressValue',
	displayCompleting: '=displayCompleting'
      },
      templateUrl: 'uploadProgress.html',
      };
  }
]);

