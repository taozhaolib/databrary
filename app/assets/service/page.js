'use strict';

module.factory('pageService', [
  '$injector', function ($injector) {
    var page = {
      auth: $injector.get('authService'),
      browser: $injector.get('browserService'),
      constants: $injector.get('constantService'),
      display: $injector.get('displayService'),
      events: $injector.get('eventService'),
      messages: $injector.get('messageService'),
      models: $injector.get('modelService'),
      assets: $injector.get('assetService'),
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
      '$window'
    ].forEach(function (dependency) {
      page[dependency] = $injector.get(dependency);
    });

    //

    page.$w = $(page.$window);

    //

    page.$rootScope.page = page;

    return page;
  }
]);
