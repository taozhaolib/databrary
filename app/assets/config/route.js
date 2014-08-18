'use strict';

module.run([
  'pageService', function (page) {
    if (page.constants.locked) {
      page.$rootScope.$on('$routeChangeStart', function (event, next) {
        if (page.auth.isLoggedIn()) {
          if (!page.auth.isAuthorized()) {
            if (!next.$$route) {
              page.$location.url(page.router.register());
            }

            var controller = angular.isFunction(next.$$route.controller) ? next.$$route.controller() : next.$$route.controller;

            if (controller !== 'registerView') {
              page.$location.url(page.router.register());
            }
          } else if (!next.authenticate && next.$$route.controller !== 'errorView' && next.$$route.originalPath !== '/profile') {
            page.$location.url(page.router.index());
          }
        } else {
          if (page.auth.isPasswordPending() && next.$$route && next.$$route.controller != 'registerView' && (!angular.isFunction(next.$$route.controller) || next.$$route.controller() != 'registerView')) {
            page.$location.url(page.router.register());
          } else if (next.authenticate) {
            page.$location.url(page.router.index());
          }
        }
      });
    }
  }
]);


