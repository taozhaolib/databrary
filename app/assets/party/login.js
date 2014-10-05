'use strict';

app.controller('party/login', [
  'pageService', function (page) {
    page.display.title = page.constants.message('login.title');
  }
]);
