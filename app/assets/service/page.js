'use strict';

module.factory('pageService', [
  '$injector', function ($injector) {
    var page = {
      analytics: $injector.get('analyticService'),
      auth: $injector.get('authService'),
      browser: $injector.get('browserService'),
      constants: $injector.get('constantService'),
      display: $injector.get('displayService'),
      events: $injector.get('eventService'),
      messages: $injector.get('messageService'),
      models: $injector.get('modelService'),
      panels: $injector.get('panelService'),
      router: $injector.get('routerService'),
      slotClock: $injector.get('slotClockService'),
      storage: $injector.get('storageService'),
      tooltips: $injector.get('tooltipService'),
    };

    //

    page.permission = page.constants.permissionName;
    page.classification = page.constants.classificationName;
    page.consent = page.constants.consentName;
    page.category = page.constants.categoryName;
    page.metric = page.constants.metricName;

    //

    angular.forEach([
      '$anchorScroll',
      '$animate',
      '$cacheFactory',
      '$compile',
      '$document',
      '$filter',
      '$http',
      '$injector',
      '$interpolate',
      '$interval',
      '$location',
      '$log',
      '$parse',
      '$q',
      '$rootScope',
      '$route',
      '$routeParams',
      '$sce',
      '$templateCache',
      '$timeout',
      '$window'
    ], function (dependency) {
      page[dependency] = $injector.get(dependency);
    });

    //

    page.$b = $('body');
    page.$m = $('main');
    page.$d = $(page.$document);
    page.$w = $(page.$window);

    //

    page.$rootScope.page = page;

    return page;
  }
]);
