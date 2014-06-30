module.factory('Scraper', [
	'$q', '$http', 'messageService', 'constantService', function ($q, $http, messages, constants) {
		return function (url) {
			var deferred = $q.defer();

			var browser = navigator.userAgent;
			var ie = 99;

			if (browser.indexOf("MSIE") > 1) {
				ie = parseInt(browser.substr(browser.indexOf("MSIE") + 5, 5));
			}

			if (ie < 10) {
				var xdr = new XDomainRequest();
				xdr.open("GET", url);

				xdr.onload = function () {
					deferred.resolve(xdr.responseText);
				};

				xdr.onerror = function () {
					page.messages.addError({
						body: page.constants.message('scraper.error', url.split('/').pop())
					});

					deferred.reject();
				};

				xdr.onprogress = function () {
				};

				xdr.ontimeout = function () {
				};

				setTimeout(function () {
					xdr.send();
				}, 0);
			} else {
				$http
					.get(url)
					.success(function (res) {
						deferred.resolve(res);
					})
					.error(function (errors, status, headers, config) {
						page.messages.addError({
							body: page.constants.message('scraper.error', config.url.split('/').pop()),
							errors: errors,
							status: status
						});

						deferred.reject();
					});
			}

			return deferred.promise;
		};
	}
]);
