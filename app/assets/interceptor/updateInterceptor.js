'use strict';

module.factory('updateInterceptor', [
	'$rootScope', function ($rootScope) {
		var version;
		var warning = false;

		return {
			response: function (res) {
				if (!res.headers) {
					return res;
				}

				var newVersion = res.headers().server;

				if (!newVersion) {
					return res;
				}

				newVersion = newVersion.split('/').pop();

				if (!warning && typeof version !== 'undefined' && newVersion !== version) {
					$rootScope.$emit('displayService-updateApp');
					warning = true;
				}

				version = newVersion;

				return res;
			}
		};
	}
]);

module.config([
	'$httpProvider', function ($httpProvider) {
		$httpProvider.interceptors.push('updateInterceptor');
	}
]);
