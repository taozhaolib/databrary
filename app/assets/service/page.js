'use strict';

module.factory('pageService', [
  '$injector', function ($injector) {
    var page = {
      auth: $injector.get('authService'),
      constants: $injector.get('constantService'),
      display: $injector.get('displayService'),
      messages: $injector.get('messageService'),
      models: $injector.get('modelService'),
      router: $injector.get('routerService'),
      tooltips: $injector.get('tooltipService'),
    };

    //

    page.permission = page.constants.permission;
    page.classification = page.constants.classification;

    //

    [
      '$filter',
      '$location',
      '$parse',
      '$q',
      '$rootScope',
      '$route',
      '$sce',
      '$timeout',
    ].forEach(function (dependency) {
      page[dependency] = $injector.get(dependency);
    });

    //

    page.$w = $($injector.get('$window'));

    //

    page.$rootScope.page = page;

    return page;
  }
]);
