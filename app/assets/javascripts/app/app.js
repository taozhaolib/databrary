require.config({
	paths: {
		'jquery': [
			'//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min',
			'vendors/jquery-1.10.2/jquery.min'
		],
		'angular': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular.min',
			'vendors/angular-1.2.0/angular.min'
		],
		'angular-animate': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-animate.min',
			'vendors/angular-1.2.0/angular-animate.min'
		],
		'angular-cookies': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-cookies.min',
			'vendors/angular-1.2.0/angular-cookies.min'
		],
		'angular-loader': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-loader.min',
			'vendors/angular-1.2.0/angular-loader.min'
		],
		'angular-resource': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-resource.min',
			'vendors/angular-1.2.0/angular-resource.min'
		],
		'angular-route': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-route.min',
			'vendors/angular-1.2.0/angular-route.min'
		],
		'angular-sanitize': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-sanitize.min',
			'vendors/angular-1.2.0/angular-sanitize.min'
		],
		'angular-touch': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-touch.min',
			'vendors/angular-1.2.0/angular-touch.min'
		],
		'angular-mocks': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-mocks',
			'vendors/angular-1.2.0/angular-mocks'
		],
		'angular-scenario': [
			'//ajax.googleapis.com/ajax/libs/angularjs/1.2.0/angular-scenario',
			'vendors/angular-1.2.0/angular-scenario'
		]
	},

	shim: {
		'jquery': {exports: 'jQuery'},
		'angular': {exports: 'angular'},
		'angular-animate': {deps: ['angular']},
		'angular-cookies': {deps: ['angular']},
		'angular-loader': {deps: ['angular']},
		'angular-resource': {deps: ['angular']},
		'angular-route': {deps: ['angular']},
		'angular-sanitize': {deps: ['angular']},
		'angular-touch': {deps: ['angular']},
		'angular-mocks': {deps: ['angular']},
		'angular-scenario': {deps: ['angular']}
	}
});

require(['jquery', 'angular'], function ($, angular) {
	console.log($, angular);
//	$(function () {
//		angular.bootstrap(document, ['mainApp']);
//	});
});
