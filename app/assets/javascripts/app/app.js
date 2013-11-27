require.config({
	baseUrl: '/public/javascripts',

	paths: {
		// requirejs, third-party

		'domReady': [
			'vendors/domReady-2.0.1/domReady'
		],

		// jquery

		'jquery': [
			'//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min',
			'vendors/jquery-1.10.2/jquery.min'
		],

		// angular

		'angular': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular.min',
			'vendors/angular-1.2.0/angular.min'
		],
		'ngAnimate': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-animate.min',
			'vendors/angular-1.2.0/angular-animate.min'
		],
		'ngCookies': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-cookies.min',
			'vendors/angular-1.2.0/angular-cookies.min'
		],
		'ngLoader': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-loader.min',
			'vendors/angular-1.2.0/angular-loader.min'
		],
		'ngResource': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-resource.min',
			'vendors/angular-1.2.0/angular-resource.min'
		],
		'ngRoute': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-route.min',
			'vendors/angular-1.2.0/angular-route.min'
		],
		'ngSanitize': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-sanitize.min',
			'vendors/angular-1.2.0/angular-sanitize.min'
		],
		'ngTouch': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-touch.min',
			'vendors/angular-1.2.0/angular-touch.min'
		],

		'ngMocks': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-mocks',
			'vendors/angular-1.2.0/angular-mocks'
		],
		'ngScenario': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-scenario',
			'vendors/angular-1.2.0/angular-scenario'
		],

		// angular, third-party

		'ngStorage': [
			'vendors/ngStorage/ngStorage.min'
		]
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

		'ngStorage': {deps: ['angular']}
	},

	deps: [
		'app/config/bootstrap'
	]
});
