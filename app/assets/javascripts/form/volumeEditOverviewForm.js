module.directive('volumeEditOverviewForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditOverviewForm;

			form.data = {};
			var backup = {};

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.resetFn = undefined;
			form.cancelFn = undefined;

			//

			form.init = function (data, volume) {
				form.data = data;
				form.volume = form.volume || volume;
				backup = $.extend(true, {}, data);
			};

			//

			form.save = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				if (form.volume) {
					page.models.Volume.save(form.data,
						function (res) {
							page.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.overview.success'),
							});

							if (angular.isFunction(form.successFn)) {
								form.successFn(form, res);
							}

							form.$setPristine();
							page.models.Volume.$cache.removeAll();
						}, function (res) {
							page.messages.addError({
								body: page.constants.message('volume.edit.overview.error'),
								report: res
							});

							if (angular.isFunction(form.errorFn)) {
								form.errorFn(form, res);
							}
						});
				} else {
					var volume = new page.models.Volume(form.data);

					volume.$save({
						owner: page.auth.user.id
					}, function (res) {
						page.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.overview.success'),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						page.$location.url(page.router.volumeEdit(res));
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('volume.edit.overview.error'),
							report: res
						});

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}
					});
				}
			};

			//

			form.autoDOI = function () {
				if (!angular.isUndefined(form.hasCitations)) {
					form.hasCitations = false;

					for (var cite in form.volume.citations) {
						if (form.volume.citations.hasOwnProperty(cite)) {
							form.hasCitations = true;
							break;
						}
					}
				}

				var doi = page.constants.data.regex.doi.exec(form.data.doi);

				if (!form.data.doi || !doi || !doi[1]) {
					return;
				}

				if (!form.data.name) {
					page.models.CrossCite
						.json(doi[1])
						.then(function (res) {
							if (!res.title) {
								page.messages.add({
									type: 'red',
									countdown: 3000,
									body: page.constants.message('volume.edit.autodoi.name.error'),
								});
							} else {
								form.data.name = res.title;

								page.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.autodoi.name.success'),
								});
							}
						}, function (res) {
							page.messages.add({
								type: 'red',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.name.error'),
							});
						});
				}

				if (!form.hasCitations) {
					page.models.CrossCite
						.apa(doi[1])
						.then(function (res) {
							form.data.study = {
								url: 'doi:' + doi[1],
								head: res,
								body: '',
							};

							page.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.citation.success'),
							});
						}, function (res) {
							page.messages.add({
								type: 'red',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.citation.error'),
							});
						});
				}
			};

			//

			page.events.talk('volumeEditOverviewForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditOverviewForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
