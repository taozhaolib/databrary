define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PartyAuthorize', ['$rootScope', 'resourceService', function ($rootScope, resourceService) {
		return resourceService('partyAuthorize', '/api/party/:partyId/authorize', {
			id: '@id'
		}, []);
	}]);
});
