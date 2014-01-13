define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Party', ['$rootScope', 'ResourceService2', function ($rootScope, resourceService) {
		return resourceService('/api/party/:id', {
			id: '@id'
		}, [], {
			watch: [
				'volumes',
				'comments',
				'parents',
				'children',
//				'tags',
//				'network',
				'funding'
			]
		});
	}]);
});
