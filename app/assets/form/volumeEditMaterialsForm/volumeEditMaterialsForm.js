module.directive('volumeEditMaterialsForm', [
	'pageService', function (page) {
		var link = function ($scope, $el, $attrs) {
			var form = $scope.volumeEditMaterialsForm;
			form.excerptsMode = $attrs.mode === 'excerpts';

			form.data = {};
			form.volume = undefined;
			form.slot = undefined;
			var backup = {};

			form.saveFn = undefined;
			form.addFn = undefined;
			form.removeFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			//

			form.init = function (data, volume) {
				form.data = data;

				if (!form.data.assets) {
					form.data.assets = [];
				}

				form.volume = form.volume || volume;
			};

			form.filterAssets = function () {
				if (!form.slot) {
					return [];
				}

				return page.$filter('filter')(form.slot.assets, function (asset) {
					var e = angular.isDefined(asset.excerpt);
					if (!asset.classification) {
						asset.classification = page.constants.data.classification[e ? asset.excerpt : asset.asset.classification];
					}
					return e === form.excerptsMode;
				});
			};

			form.saveText = function (subform) {
				return subform.asset.asset && subform.asset.asset.creation ? page.constants.message('save') : page.constants.message('upload');
			};

			form.disableButton = function (subform) {
				if (subform.form.$dirty) {
					if (subform.asset.asset && subform.asset.asset.creation) {
						return false;
					} else if (!subform.asset.file || subform.asset.file.length === 0) {
						form.clean(subform);
					} else {
						return false;
					}
				}

				return true;
			};

			//

			form.save = function (subform) {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form, subform);
				}

				var classification = page.classification[form.excerptsMode ? 'RESTRICTED' : subform.asset.classification];
				var excerpt = form.excerptsMode ? page.classification[subform.asset.classification] : '';
				if (subform.asset.file) {
					var fd = new FormData();
					fd.append('file', subform.asset.file[0]);
					fd.append('name', subform.asset.name || '');
					fd.append('classification', classification);
					fd.append('excerpt', excerpt);
					fd.append('container', form.slot.container.id);

					var msg = subform.messages.add({
						type: 'yellow',
						body: page.constants.message('volume.edit.materials.create', subform.asset.name || page.constants.message('file')),
					});

					if (subform.asset.asset) {
						page.models.Asset.replace(subform.asset, fd)
							.then(function (res) {
								subform.messages.add({
									type: 'green',
									closeable: true,
									body: page.constants.message('volume.edit.materials.replace.success', subform.asset.name || page.constants.message('file')),
								});

								if (angular.isFunction(form.successFn)) {
									form.successFn(form, res);
								}

								delete subform.asset.file;
								subform.asset.asset = res.data.asset;
								page.models.Asset.get({
									creation: '',
									id: res.data.asset.id
								}, function (res) {
									subform.asset.asset.creation = res.creation;
									form.store(subform);
									form.clean(subform);
								});

								page.models.Slot.$cache.removeAll();
							}, function (res) {
								subform.messages.addError({
									type: 'red',
									body: page.constants.message('volume.edit.materials.replace.error', subform.asset.name || page.constants.message('file')),
									report: res,
								});

								if (angular.isFunction(form.errorFn)) {
									form.errorFn(form, res);
								}
							}).finally(function () {
								subform.messages.remove(msg);
								form.clean(subform);
								page.display.scrollTo(subform.$element);
							});
					} else {
						page.models.Asset.upload(form.volume, fd)
							.then(function (res) {
								subform.messages.add({
									type: 'green',
									closeable: true,
									body: page.constants.message('volume.edit.materials.create.success', subform.asset.name || page.constants.message('file')),
								});

								subform.messages.remove(msg);

								if (angular.isFunction(form.successFn)) {
									form.successFn(form, res);
								}

								delete subform.asset.file;
								subform.asset.asset = res.data.asset;
								page.models.Asset.get({
									creation: '',
									id: res.data.asset.id
								}, function (res) {
									subform.asset.asset.creation = res.creation;
									form.store(subform);
									form.clean(subform);
								});

								form.clean(subform);
								page.models.Slot.$cache.removeAll();
							}, function (res) {
								subform.messages.addError({
									type: 'red',
									body: page.constants.message('volume.edit.materials.create.error', subform.asset.name || page.constants.message('file')),
									report: res,
								});

								if (angular.isFunction(form.errorFn)) {
									form.errorFn(form, res);
								}

								subform.messages.remove(msg);

								form.clean(subform);
								page.display.scrollTo(subform.$element);
							});
					}
				} else {
					var newAsset = new page.models.Asset({
						name: subform.asset.name || '',
						classification: classification,
						excerpt: excerpt,
					});

					newAsset.$save({
						id: subform.asset.asset.id
					}).then(function (res) {
						subform.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.materials.update.success', subform.asset.name || page.constants.message('file')),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.clean(subform);
						form.store(subform);
						page.models.Slot.$cache.removeAll();
					}, function (res) {
						subform.messages.addError({
							type: 'red',
							body: page.constants.message('volume.edit.materials.update.error', subform.asset.name || page.constants.message('file')),
							report: res,
						});

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}

						form.clean(subform);
						page.display.scrollTo(subform.$element);
					});
				}
			};

			form.saveAll = function () {
				angular.forEach(form, function (subform, id) {
					if (id.indexOf('asset-') === 0 && form[id].$dirty) {
						form.save(subform.subform);
					}
				});
			};

			form.replace = function (subform) {
				delete subform.asset.asset.creation;
				form.$setDirty();
			};

			form.remove = function (subform) {
				if (angular.isFunction(form.removeFn)) {
					form.removeFn(form, subform);
				}

				if (!subform.asset.asset) {
					form.clean(subform);
					form.data.assets.splice(form.data.assets.indexOf(subform.asset), 1);
				} else {
					var newAsset = new page.models.Asset();

					newAsset.$delete({
						id: subform.asset.asset.id
					}).then(function (res) {
						form.messages.add({
							type: 'green',
							countdown: 3000,
							body: page.constants.message('volume.edit.materials.remove.success', subform.asset.name || page.constants.message('file')),
						});

						if (angular.isFunction(form.successFn)) {
							form.successFn(form, res);
						}

						form.data.assets.splice(form.data.assets.indexOf(subform.asset), 1);

						form.clean(subform);
						page.models.Slot.$cache.removeAll();
					}, function (res) {
						form.messages.addError({
							type: 'red',
							body: page.constants.message('volume.edit.materials.remove.error', subform.asset.name || page.constants.message('file')),
							report: res,
						});

						if (angular.isFunction(form.errorFn)) {
							form.errorFn(form, res);
						}

						form.clean(subform);
						page.display.scrollTo(subform.$element);
					});
				}
			};

			form.add = function () {
				if (angular.isFunction(form.addFn)) {
					form.saveFn(form);
				}

				return form.data.assets.push({
					classification: 'SHARED',
					excerpt: form.excerptsMode ? page.classification.SHARED : undefined
				});
			};

			form.clean = function (subform) {
				if (subform) {
					subform.form.$setPristine();
				}

				var pristine = true;

				angular.forEach(form, function (subform, id) {
					if (id.indexOf('asset-') === 0 && form[id] && form[id].$dirty) {
						pristine = false;
						return false;
					}
				});

				if (pristine) {
					form.$setPristine();
				}
			};

			form.store = function (subform) {
				backup[subform.$id] = $.extend(true, {}, subform.asset);

				var subwatch = subform.$watch('form.name.validator', function (val) {
					if (!val) {
						return;
					}

					subform.form.name.validator.client({
						tips: page.constants.message('material.name.help'),
					}, true);

					subwatch();
				});
			};

			form.reset = function (subform) {
				subform.asset = backup[subform.$id];

				if (subform.asset.asset) {
					page.models.Asset.get({
						creation: '',
						id: subform.asset.asset.id
					}, function (res) {
						subform.asset.asset.creation = res.creation;
					});
				} else {
					form.remove(subform);
				}

				form.store(subform);
				form.clean(subform);
			};

			form.resetAll = function () {
				angular.forEach(form, function (subform, id) {
					if (id.indexOf('asset-') === 0 && form[id].$dirty) {
						form.reset(subform.subform);
					}
				});
			};

			//

			page.events.talk('volumeEditMaterialsForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditMaterialsForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
