'use strict';

module.controller('loginView', [
  'pageService', function (page) {
    page.display.title = page.constants.message('page.title.login');
  }
]);
