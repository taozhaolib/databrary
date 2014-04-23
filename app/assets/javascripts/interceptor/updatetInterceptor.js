module.factory('updateInterceptor', ['$rootScope', function ($rootScope) {
	var version = undefined;

	return {
		response: function (res) {
			if (!res.headers)
				return res;

			var newVersion = res.headers().server;

			if (!newVersion)
				return res;

			newVersion = newVersion.split('/').pop();

			if (typeof version !== 'undefined' && newVersion !== version) {
				$rootScope.$emit('pageService-updateApp');
			}

			version = newVersion;

			return res;
		}
	}
}]);
