'use strict';

app.factory('displayService', [
  '$rootScope', 'storageService', '$filter', 'messageService', 'tooltipService', 'constantService', '$timeout', '$window',
  function ($rootScope, storage, $filter, messages, tooltips, constants, $timeout, window) {
    var display = {};

    display.title = '';

    display.loading = false;

    $rootScope.$on('$routeChangeStart', function () {
      display.loading = true;
      display.error = false;
      tooltips.clear();
      messages.clear();
    });

    $rootScope.$on('$routeChangeSuccess', function () {
      display.loading = false;
    });

    display.error = undefined;

    $rootScope.$on('$routeChangeError', function (event, next, previous, error) {
      display.error = true;
      display.loading = false;
      display.scrollTo(0);
      $rootScope.$broadcast('displayService-error', error);
    });

    var $scroll = $('html,body');

    display.scrollTo = function (target) {
      $timeout(function () {
        if (angular.isFunction(target))
          target = target();
        if (angular.isString(target))
          target = $(target);
        if (!angular.isNumber(target)) {
          if (!target.length) return;
          target = target.offset().top - 72;
        }
        $scroll.animate({
          scrollTop: target
        }, 500);
      }, 1);
    };

    var ageKeys = ['science', 'days', 'months', 'years'],
      ageKey = storage.get('displayAge') || 'science';

    display.toggleAge = function () {
      ageKey = ageKeys[(ageKeys.indexOf(ageKey) + 1) % ageKeys.length];
      $rootScope.$broadcast('displayService-toggleAge', ageKey);
      storage.set('displayAge', ageKey);
    };

    display.formatAge = function (value) {
      return $filter('age')(value, ageKey);
    };

    /*$routeChangeStart is always fires before $locationChangeStart*/ 
    display.cancelRouteChange = function(event){
      display.loading = false;
      event.preventDefault();
    };

    /* TODO: this should really use .canPlayType */
    if (window.navigator.userAgent.search(/^Mozilla\/.* \(Macintosh; .* Firefox\/([0-2]|[3][0-4])/) === 0)
      messages.add({
        type: 'yellow',
        body: constants.message('video.unsupported'),
        persist: true
      });

    return display;
  }
]);
