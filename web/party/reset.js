'use strict';

app.controller('party/reset', [
  '$scope', 'pageService',
  function ($scope, page) {
    page.display.title = page.constants.message('reset.title');
  }
]);
