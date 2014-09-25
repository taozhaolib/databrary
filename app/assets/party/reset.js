'use strict';

module.controller('party/reset', [
  '$scope', 'pageService', function ($scope, page) {
    page.display.title = page.constants.message('page.title.reset');

    //

    $scope.$on('userPasswordForm-init', function (event, form) {
      form.saveSuccessFn = form.resetSuccessFn = function () {
        page.$location.url(page.router.index());
      };

      event.stopPropagation();
    });
  }
]);
