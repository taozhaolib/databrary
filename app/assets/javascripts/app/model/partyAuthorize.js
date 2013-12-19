define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PartyAuthorize', ['$rootScope', 'ResourceService', function ($rootScope, resourceService) {
		return resourceService('partyAuthorize', '/api/party/:partyId/authorize', {
			id: '@id'
		}, []);
	}]);
});
