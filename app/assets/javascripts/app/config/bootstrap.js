define(['require', 'angular', 'app/modules/dbModule', './routes'], function (require, angular) {
	'use strict';

	require(['domReady!'], function (document) {
		angular.bootstrap(document, ['dbModule']);
	});
});
