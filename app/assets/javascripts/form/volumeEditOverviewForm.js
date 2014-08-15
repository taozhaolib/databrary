module.directive('volumeEditOverviewForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditOverviewForm;

			form.data = {};
			form.authors = [
				{
					name: '',
				}
			];
			var backup = {};

			//

			form.setAutomatic = function (auto) {
				form.automatic = auto;
				form.validator.client({
					'citation.url': {
						tips: page.constants.message('volume.edit.citation.' + (form.automatic ? 'doi' : 'url') + '.help')
					}
				}, true);
			};

			form.init = function (data, volume) {
				form.data = data;
				form.volume = form.volume || volume;
				form.setAutomatic(!form.volume);

				if (!form.data.citation) {
					form.data.citation = {};
				}

				if (form.data.citation && form.data.citation.authors) {
					form.authors = form.data.citation.authors.map(function (author) {
						return {
							name: author,
						};
					});

					if (form.authors.length === 0) {
						form.authors.push({
							name: '',
						});
					}
				}

				backup = $.extend(true, {}, data);
			};

			//

			form.save = function () {
				if (!form.data.citation) {
					form.data.citation = {};
				}

				form.data.citation.authors = form.authors.map(function (author) {
					return author.name.trim();
				}).filter(function (author) {
					return author != '';
				});

				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				if (form.volume) {
					page.models.Volume.save(form.data,
						function (res) {
							form.validator.server({});

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
							form.validator.server(res);
							page.display.scrollTo(form.$element);

							if (angular.isFunction(form.errorFn)) {
								form.errorFn(form, res);
							}
						});
				} else {
					var volume = new page.models.Volume(form.data);

					volume.$save({
						owner: page.$routeParams.owner ||  page.auth.user.id
					}, function (res) {
						form.validator.server({});

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
						page.$location.url(page.router.volumeEdit(res));
					}, function (res) {
						form.validator.server(res);

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}
					});
				}
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form);
				}

				form.data = $.extend(true, {}, backup);

				form.validator.clearServer();

				if (backup.citation && backup.citation.authors) {
					form.authors = backup.citation.authors.map(function (author) {
						return {
							name: author,
						};
					});
				} else {
					form.authors = [];
				}

				form.$setPristine();
			};

			//

			form.autoDOI = function () {
				var doi = page.constants.regex.doi.exec(form.data.citation.url);

				if (!doi || !doi[1]) {
					return;
				}

				page.models.Cite(doi[1])
					.then(function (res) {
						form.data.citation = res;
						form.data.name = res.title;

						if (res.authors) {
							form.authors = [];

							angular.forEach(res.authors, function (author) {
								form.authors.push({name: author});
							});
						}

						page.$timeout(function () {
							form['name'].$setViewValue(res.title);
							form['citation.url'].$setViewValue(res.url);
							form['citation.head'].$setViewValue(res.head);
							form['citation.year'].$setViewValue(res.year);
						});

						form.setAutomatic(false);

						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.autodoi.citation.success'),
						});
					}, function (res) {
						form.messages.add({
							type: 'red',
							countdown: 5000,
							body: page.constants.message('volume.edit.autodoi.citation.error'),
						});
					});
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

				form.$setDirty();
			};

			//

			form.validator.client({
				name: {
					tips: page.constants.message('volume.edit.name.help')
				},
				body: {
					tips: page.constants.message('volume.edit.body.help')
				},
				alias: {
					tips: page.constants.message('volume.edit.alias.help')
				},
				'citation.head': {
					tips: page.constants.message('volume.edit.citation.head.help')
				},
				'citation.url': {
					tips: page.constants.message('volume.edit.citation.url.help'),
				},
				'citation.year': {
					tips: page.constants.message('volume.edit.citation.year.help')
				},
			}, true);

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
