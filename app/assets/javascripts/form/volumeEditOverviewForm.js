module.directive('volumeEditOverviewForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditOverviewForm;

			form.data = {};
			form.authors = [];
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
				form.automatic = !form.volume;

				if (!form.data.citation) {
					form.data.citation = {};
				}

				if (form.data.citation && form.data.citation.authors) {
					form.authors = form.data.citation.authors.map(function (author) {
						return {
							name: author,
						};
					});
				}

				backup = $.extend(true, {}, data);
			};

			//

			form.save = function () {
				if (!form.data.citation) {
					form.data.citation = {};
				}

				if (form.authors.length > 0) {
					form.data.citation.authors = form.authors.map(function (author) {
						return author.name;
					});
				}

				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				if (form.volume) {
					page.models.Volume.save(form.data,
						function (res) {
							form.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.overview.success'),
							});

							backup = $.extend(true, {}, form.data);

							if (angular.isFunction(form.successFn)) {
								form.successFn(form, res);
							}

							form.$setPristine();
							page.models.Volume.$cache.removeAll();
						}, function (res) {
							form.validator.server(res, true);

							if (angular.isFunction(form.errorFn)) {
								form.errorFn(form, res);
							}
						});
				} else {
					var volume = new page.models.Volume(form.data);

					volume.$save({
						owner: page.auth.user.id
					}, function (res) {
						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.overview.success'),
						});
						//update backup so a future revert goes to current state, not pageload state
						backup = $.extend(true, {}, form.data);

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.$setPristine();
						page.$location.url(page.router.volumeEdit(res));
					}, function (res) {
						form.messages.addError({
							body: page.constants.message('volume.edit.overview.error'),
							report: res
						});

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}
					});
				}
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn))
					form.resetFn(form);

				form.data = $.extend(true, {}, backup);
				form.$setPristine();
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

				var doi = page.constants.regex.doi.exec(form.data.citation.url);

				if (!doi || !doi[1]) {
					return;
				}

				page.models.CrossCite
					.json(doi[1])
					.then(function (res) {
						if (!res.title) {
							form.messages.add({
								type: 'red',
								body: page.constants.message('volume.edit.autodoi.name.error'),
							});
						} else {
							form.data.name = res.title;

							if (!form.data.citation) {
								form.data.citation = {};
							}

							if (res.issued && res.issued['date-parts'] && res.issued['date-parts'][0] && res.issued['date-parts'][0][0]) {
								form.data.citation.year = res.issued['date-parts'][0][0];
							}

							if (res.author) {
								var parts = ['given', 'non-dropping-particle', 'family', 'suffix'];

								form.authors = [];

								angular.forEach(res.author, function (author) {
									var name = '';

									angular.forEach(parts, function (part) {
										if (author[part]) {
											name += author[part] + ' ';
										}
									});

									name = name.slice(0, -1);

									form.authors.push({name: name});
								});
							}

							form.automatic = false;

							form.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.name.success'),
							});
						}
					}, function (res) {
						form.messages.add({
							type: 'red',
							body: page.constants.message('volume.edit.autodoi.name.error'),
						});
					});

				if (!form.hasCitations) {
					page.models.CrossCite
						.apa(doi[1])
						.then(function (res) {
							if (!form.data.citation) {
								form.data.citation = {};
							}

							form.data.citation.url = 'doi:' + doi[1];
							form.data.citation.head = res;

							form.automatic = false;

							form.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.citation.success'),
							});
						}, function (res) {
							form.messages.add({
								type: 'red',
								body: page.constants.message('volume.edit.autodoi.citation.error'),
							});
						});
				}
			};

			//

			form.addAuthor = function () {
				if (!form.authors) {
					form.authors = [];
				}

				form.authors.push({});
			};

			form.removeAuthor = function (author) {
				var i = form.authors.indexOf(author);

				if (i > -1) {
					form.authors.splice(i, 1);
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
