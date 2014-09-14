'use strict';

module.factory('displayService', [
  '$rootScope', 'storageService', 'eventService', '$filter', 'messageService', 'constantService', '$timeout', '$window',
  function ($rootScope, storage, events, $filter, messages, constants, $timeout, $window) {
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
      display.toolbarLinks = [];
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
      ageKey = storage.get('displayAge') || 'science';

    display.toggleAge = function () {
      ageKey = ageKeys[(ageKeys.indexOf(ageKey) + 1) % ageKeys.length];
      events.talk('displayService-toggleAge', ageKey);
      storage.set('displayAge', ageKey);
    };

    display.formatAge = function (value) {
      return $filter('age')(value, ageKey);
    };

    //

    if ($window.navigator.userAgent.toLowerCase().contains('firefox') &&
	$window.navigator.platform.toLowerCase().contains('mac'))
      messages.add({
	type: 'yellow',
	closeable: true,
	body: constants.message('video.unsupported')
      });

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
