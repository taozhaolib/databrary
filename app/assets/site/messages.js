'use strict';

app.directive('messages', [
  'pageService', function (page) {
    var controller = ['$scope', function ($scope) {
      return ($scope.messages = {
        list: page.messages.list
      });
    }];

    //

    return {
      restrict: 'E',
      templateUrl: 'site/messages.html',
      controller: controller,
      controllerAs: 'messages',
    };
  }
]);
