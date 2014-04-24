module.factory('Tag', [
	'resourceService', function (resource) {
		return resource('tag', '/api/tag/:id', {});
	}
]);
