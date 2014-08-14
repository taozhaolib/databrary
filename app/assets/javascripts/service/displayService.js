module.factory('displayService', [
	'$rootScope',
	'$sessionStorage',
	'eventService',
	'$filter',
	'messageService',
	'constantService',
	'routerService',
	'$location',
	'$timeout',
	'$window',
	function ($rootScope, $sessionStorage, events, $filter, messages, constants, router, $location, $timeout, $window) {
		var display = {};

		//

		display.title = 'Welcome!';

		//

		display.loading = false;

		$rootScope.$on('$routeChangeStart', function () {
			display.loading = true;
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
			display.error = error;
			$location.url(router.error());
		});

		//

		events.listen($rootScope, 'displayService-updateApp', function () {
			messages.add({
				type: 'yellow',
				body: constants.message('app.update') + ' <a href="" onclick="window.location.reload()">Reload</a>.'
			});
		});

		//

		display.toolbarLinks = [];

		//

		var $scroll = $('html,body');

		display.scrollTo = function (id) {
			$timeout(function () {

				var target = (angular.isString(id) ? $('#' + id) : id).offset().top - 72;
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

			if (result === true) {
				return display.navigationFn = undefined;
			} else if (angular.isUndefined(result)) {
				return;
			}

			if (result === true || confirm(constants.message('navigation.confirmation'))) {
				return display.navigationFn = undefined;
			}

			event.preventDefault();
		});

		//

		var ageKeys = ['science', 'days', 'months', 'years'],
			ageKey = $sessionStorage['displayAge'] || 'science';

		display.toggleAge = function () {
			ageKey = ageKeys[(ageKeys.indexOf(ageKey) + 1) % ageKeys.length];
			events.talk('displayService-toggleAge', ageKey);
			$sessionStorage['displayAge'] = ageKey;
		};

		display.formatAge = function (value) {
			return $filter('age')(value, ageKey);
		};

		//
		
		display.getUA = function() {return $window.navigator.userAgent;};

		display.getPlatform = function() {return $window.navigator.platform;};

		display.isInternetExplorer = function() {
			return /msei/i.test(display.getUA()) ||
			  /trident/i.test(display.getUA()) && $window.navigator.appName == 'Netscape';
		};

		display.videoSupported = function() {
		        //the only non-supporting browser, as far as we know, is firefox on mac
		        //so the only false response is when both match
		        return !(/mac/i.test(display.getPlatform()) &&  /firefox/i.test(display.getUA()));
		};

		return display;
	}
]);
