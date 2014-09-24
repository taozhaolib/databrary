'use strict';

module.directive('errors', [
  'pageService', function (page) {
    var controller = ['$scope', function ($scope) {
      var that = this;

      page.events.listen($scope, 'displayService-error', function (event, error) {
        page.display.title = page.constants.message('page.title.error');
        that.error = error;
      });
    }];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'site/errors.html',
      controller: controller,
      controllerAs: 'errors',
    };
  }
]);
