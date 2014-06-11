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
	function ($rootScope, $sessionStorage, events, $filter, messages, constants, router, $location, $timeout) {
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
				$scroll.scrollTop($('#' + id).offset().top - 72);
			}, 1);
		};

		//

		display.navigationFn = undefined;

		$rootScope.$on('$locationChangeStart', function (event, url) {
			if (!angular.isFunction(display.navigationFn) || display.navigationFn(event, url) || confirm(constants.message('navigation.confirmation'))) {
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

		return display;
	}
]);
