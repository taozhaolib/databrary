require.config({
	baseUrl: '/public/javascripts',

	paths: {
		// requirejs, third-party

		'domReady': 'vendors/domReady-2.0.1/domReady',

		// jquery

		'jquery': 'vendors/jquery-1.10.2/jquery.min',

		// angular

		'angular': 'vendors/angular-1.2.9/angular.min',
		'ngAnimate': 'vendors/angular-1.2.9/angular-animate.min',
		'ngCookies': 'vendors/angular-1.2.9/angular-cookies.min',
		'ngLoader': 'vendors/angular-1.2.9/angular-loader.min',
		'ngResource': 'vendors/angular-1.2.9/angular-resource.min',
		'ngRoute': 'vendors/angular-1.2.9/angular-route.min',
		'ngSanitize': 'vendors/angular-1.2.9/angular-sanitize.min',
		'ngTouch': 'vendors/angular-1.2.9/angular-touch.min',

		'ngMocks': 'vendors/angular-1.2.9/angular-mocks',
		'ngScenario': 'vendors/angular-1.2.9/angular-scenario',

		// angular, third-party

		'ngStorage': 'vendors/ngStorage/ngStorage.min',
		'bindonce': 'vendors/bindonce/bindonce.min'
	},

	shim: {
		// jquery

		'jquery': {exports: 'jQuery'},

		// angular

		'angular': {
			exports: 'angular',
			deps: ['jquery']
		},
		'ngAnimate': {deps: ['angular']},
		'ngCookies': {deps: ['angular']},
		'ngLoader': {deps: ['angular']},
		'ngResource': {deps: ['angular']},
		'ngRoute': {deps: ['angular']},
		'ngSanitize': {deps: ['angular']},
		'ngTouch': {deps: ['angular']},

		'ngMocks': {deps: ['angular']},
		'ngScenario': {deps: ['angular']},

		// angular, third-party

		'ngStorage': {deps: ['angular']},
		'bindonce': {deps: ['angular']}
	},

	deps: [
		'app/config/bootstrap'
	]
});
