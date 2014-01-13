define(['app/config/module'], function (module) {
	'use strict';

	module.factory('PartyAuthorize', ['$rootScope', '$resource', function ($rootScope, $resource) {
		return $resource('/api/party/:partyId/authorize', {
			id: '@id'
		});
	}]);
});
