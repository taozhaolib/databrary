module.factory('Comment', [
	'resourceService', '$route', function (resource, $route) {
		return resource('comment', '/api/comment/:id', {
			segment: function () {
				return $route.current.params.segment || ',';
			},
			container: function () {
				return $route.current.params.container || ',';
			}
		});
	}
]);
