'use strict';

module.factory('tag', [
	'$resource', function ($resource) {
		return $resource('/api/tag/:id', {});
	}
]);
