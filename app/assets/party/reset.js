'use strict';

module.controller('party/reset', [
  '$scope', 'pageService', function ($scope, page) {
    page.display.title = page.constants.message('page.title.reset');

    //

    page.events.listen($scope, 'userPasswordForm-init', function (event, form) {
      form.saveSuccessFn = form.resetSuccessFn = function () {
        page.$location.url(page.router.index());
      };

      event.stopPropagation();
    });
  }
]);
