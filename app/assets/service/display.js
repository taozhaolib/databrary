'use strict';

app.factory('displayService', [
  '$rootScope', 'storageService', '$filter', 'messageService', 'constantService', '$timeout', '$window',
  function ($rootScope, storage, $filter, messages, constants, $timeout, $window) {
    var display = {};

    //

    display.title = 'Welcome!';

    //

    display.loading = false;

    $rootScope.$on('$routeChangeStart', function () {
      display.loading = true;
      display.error = false;
    });

    $rootScope.$on('$routeChangeSuccess', function () {
      display.loading = false;
      display.toolbarLinks = [];
    });

    //

    display.error = undefined;

    $rootScope.$on('$routeChangeError', function (event, next, previous, error) {
      display.error = true;
      display.loading = false;
      display.scrollTo(0);
      display.toolbarLinks = [];
      $rootScope.$broadcast('displayService-error', error);
    });

    //

    display.toolbarLinks = [];

    //

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

    //will give error if $float or $floater are not elements with offset() available
    display.makeFloatScrollFn = function(float, floater, x) {
      return function () {
        if (window.pageYOffset + window.innerHeight < float.offset().top+x){
          floater.addClass('float');
        } else
          floater.removeClass('float');
      };
    };

    //

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

    //

    /* TODO: this should really use .canPlayType */
    if ($window.navigator.userAgent.toLowerCase().contains('firefox') &&
        $window.navigator.platform.toLowerCase().contains('mac'))
      messages.add({
        type: 'yellow',
        closeable: true,
        body: constants.message('video.unsupported')
      });

    //
    return display;
  }
]);
