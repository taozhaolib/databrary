define(['app/config/module'], function (module) {
	'use strict';

	module.factory('RetryService', ['$q', '$log', function ($q, $log) {
		var retryService = {};

		var queue = [];

		//

		retryService.isPending = function () {
			return queue.length > 0;
		};

		//

		retryService.addCallbacks = [];

		var process = function (item) {
			queue.push(item);

			angular.forEach(retryService.addCallbacks, function (callback) {
				try {
					callback(item);
				} catch (e) {
					$log.error('securityRetryQueue.push(retryItem): callback error' + e);
				}
			});
		};

		//

		retryService.add = function (retryFn) {
			var defer = $q.defer();

			var success = function (reponse) {
				defer.resolve(reponse);
			};

			var error = function (response) {
				defer.reject(response);
			};

			var cancel = function () {
				defer.reject();
			};

			var item = {
				retry: function () {
					$q.when(retryFn()).then(success, error);
				},
				cancel: cancel
			};

			process(item);

			return defer.promise;
		};

		//

		retryService.retryOne = function () {
			queue.shift().retry();
		};

		retryService.retryAll = function () {
			while (retryService.isPending()) {
				retryService.retryOne();
			}
		};

		//

		retryService.cancelOne = function () {
			queue.shift().cancel();
		};

		retryService.cancelAll = function () {
			while (retryService.isPending()) {
				retryService.cancelOne();
			}
		};

		return retryService;
	}]);
});
