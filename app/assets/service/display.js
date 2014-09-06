'use strict';

module.factory('displayService', [
  '$rootScope', '$sessionStorage', 'eventService', '$filter', 'messageService', 'constantService', '$timeout', '$window', '$sce',
  function ($rootScope, $sessionStorage, events, $filter, messages, constants, $timeout, $window, $sce) {
    var display = {};

    //

    display.title = 'Welcome!';

    //

    display.loading = false;

    $rootScope.$on('$routeChangeStart', function () {
      display.loading = true;
      display.error = false;
      display.toolbarLinks = [];
    });

    $rootScope.$on('$routeChangeSuccess', function () {
      display.loading = false;
      if (display.toolbarLinks) {
        display.toolbarLinks = [];
      }
    });

    //

    display.error = undefined;

    $rootScope.$on('$routeChangeError', function (event, next, previous, error) {
      display.error = true;
      display.loading = false;
      display.scrollTo(0);
      events.talk('displayService-error', error);
    });

    //

    events.listen($rootScope, 'displayService-updateApp', function () {
      messages.add({
        type: 'yellow',
        body: $sce.trustAsHtml(constants.message('app.update') + ' ' + constants.message('app.reload'))
      });
    });

    //

    display.toolbarLinks = [];

    //

    var $scroll = $('html,body');

    display.scrollTo = function (id) {
      $timeout(function () {
        var target = angular.isNumber(id) ? id : (angular.isString(id) ? $('#' + id) : id).offset().top - 72;
        $scroll.animate({
          scrollTop: target
        }, 500);
      }, 1);
    };

    //

    display.navigationFn = undefined;

    $rootScope.$on('$locationChangeStart', function (event, url) {
      if (!angular.isFunction(display.navigationFn)) {
        return;
      }

      var result = display.navigationFn(event, url);

      if (angular.isUndefined(result)) {
        return;
      } else if (result === true || confirm(constants.message('navigation.confirmation'))) {
        return (display.navigationFn = undefined);
      }

      event.preventDefault();
    });

    //

    var ageKeys = ['science', 'days', 'months', 'years'],
      ageKey = $sessionStorage.displayAge || 'science';

    display.toggleAge = function () {
      ageKey = ageKeys[(ageKeys.indexOf(ageKey) + 1) % ageKeys.length];
      events.talk('displayService-toggleAge', ageKey);
      $sessionStorage.displayAge = ageKey;
    };

    display.formatAge = function (value) {
      return $filter('age')(value, ageKey);
    };

    //

    display.isMobile =
      typeof $window.orientation !== 'undefined';

    display.videoSupported =
      !($window.navigator.userAgent.toLowerCase().contains('firefox') && $window.navigator.platform.toLowerCase().contains('mac'));

    //

    //will give error if $float or $floater are not elements with offset() available
    display.makeFloatScrollFn = function($float, $floater, x){
      return function () {
	if (window.pageYOffset + x >= $float.offset().top) {
	  $floater.addClass('float');
	} else {
	  $floater.removeClass('float');
	}
      };
    };

    return display;
  }
]);
