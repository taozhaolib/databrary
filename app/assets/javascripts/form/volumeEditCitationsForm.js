module.directive('volumeEditCitationsForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditCitationsForm;

			form.data = {};
			form.volume = undefined;
			var backup = {};

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.addFn = undefined;
			form.removeFn = undefined;
			form.resetFn = undefined;

			//

			form.init = function (data, volume) {
				form.data = data;
				form.volume = form.volume || volume;
			};
			
			form.clean = function (subform, scrubCitations) {
				subform.$setPristine();

				var pristine = true;

				if(form.studyForm.$dirty) {
					pristine = false;
				}

				if (pristine || scrubCitations) {
					angular.forEach(form.citationsForm, function (subform, id) {
						if (id.indexOf('citation-') === 0 && form.citationsForm[id].$dirty) {
							if (scrubCitations) {
								form.citationsForm[id].$setPristine();
							} else {
								pristine = false;
								return false;
							}
						}
					});
				}

				if (pristine) {
					form.$setPristine();
				}
			};

			form.autoDOI = function (target) {
				if (!target.url) {
					return;
				}

				var doi = page.constants.regex.doi.exec(target.url);

				if (!doi || !doi[1]) {
					return;
				}

				if (!target.name) {
					page.models.CrossCite
						.apa(doi[1])
						.then(function (res) {
							if (!res) {
								form.messages.add({
									type: 'red',
									countdown: 3000,
									body: page.constants.message('volume.edit.autodoi.name.error'),
								});
							} else {
								target.head = res;

								form.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.autodoi.name.success'),
								});
							}
						}, function (res) {
							form.messages.add({
								type: 'red',
								countdown: 3000,
								body: page.constants.message('volume.edit.autodoi.name.error'),
							});
						});
				}
			};
			
			//
			
			form.studyForm.save = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				page.models.Volume.save({
						study: form.data.study,
					},
					function (res) {
						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.citations.success'),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.clean(form.studyForm);
						page.models.Volume.$cache.removeAll();
					}, function (res) {
						if (!form.validator.server(res.data)) {
							form.messages.addError({
								body: page.constants.message('volume.edit.citations.error'),
								report: res
							});
						}

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}

						form.clean(form.studyForm);
					});
			};

			form.studyForm.store = function () {
				backup.study = $.extend(true, {}, form.data.study);
			};

			form.studyForm.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form, subform);
				}

				form.data.study = backup.study;
				form.studyForm.store();
				form.clean(form.studyForm);
			};
			
			//
			
			form.citationsForm.save = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				page.models.Volume.save({
						citation: form.data.citation,
					},
					function (res) {
						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.citations.success'),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.clean(form.citationsForm, true);
						page.models.Volume.$cache.removeAll();
					}, function (res) {
						if (!form.validator.server(res.data)) {
							form.messages.addError({
								body: page.constants.message('volume.edit.citations.error'),
								report: res
							});
						}

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}

						form.clean(form.citationsForm, true);
					});
			};

			form.citationsForm.store = function (subform) {
				backup[subform.$id] = $.extend(true, {}, subform.citation);
			};

			form.citationsForm.reset = function (subform) {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form, subform);
				}

				subform.citation = backup[subform.$id];
				form.citationsForm.store(subform);
				form.clean(subform.form);
			};

			form.citationsForm.add = function () {
				if (angular.isFunction(form.addFn)) {
					form.saveFn(form);
				}

				return form.data.citation.push({});
			};

			form.citationsForm.remove = function (subform) {
				if (angular.isFunction(form.removeFn)) {
					form.removeFn(form, subform);
				}

				form.data.citations.splice(subform.$index, 1);
			};

			//

			page.events.talk('volumeEditCitationsForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditCitationsForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
