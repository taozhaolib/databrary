module.directive('volumeEditMaterialsForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.volumeEditMaterialsForm;

			form.data = {};
			form.volume = undefined;
			var backup = [];

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.resetFn = undefined;
			form.cancelFn = undefined;

			//

			form.init = function (data, volume) {
				form.data = data;
				form.volume = form.volume || volume;
				backup = data.assets.slice(0);
			};

			//

			form.save = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}

				var ids = [];
				var promises = [];

				angular.forEach(form.data.assets, function (asset) {
					asset.saving = true;

					if (asset.file) {
						var fd = new FormData();
						fd.append('file', asset.file[0]);
						fd.append('name', asset.name);
						fd.append('classification', asset.classification || 0);
						fd.append('container', form.slot.container.id);

						promises.push(page.$http
							.post('/volume/' + form.volume.id + '/asset/create', fd, {
								transformRequest: angular.identity,
								headers: {
									'Content-Type': undefined
								},
							}).success(function () {
								page.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.create.success', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}).error(function () {
								page.messages.add({
									type: 'red',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.create.error', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}));
					} else if (asset.asset) {
						ids.push(asset.asset.id);

						promises.push(page.$http
							.post('/asset/' + asset.asset.id + '/edit', {
								name: asset.name,
								classification: asset.classification,
							}).success(function () {
								page.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.update.success', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}).error(function () {
								page.messages.add({
									type: 'red',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.update.error', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}));
					}
				});

				angular.forEach(backup, function (asset) {
					if (ids.indexOf(asset.asset.id) > -1) {
						return;
					}

					promises.push(page.$http
						.post('/asset/' + asset.asset.id + '/remove')
						.success(function () {
							page.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.materials.remove.success', asset.name || asset.file[0].name),
							});

							asset.saving = false;
						}).error(function (data, status) {
							console.log(arguments);

							if (status === 404 || status === 303) {
								return page.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.remove.success', asset.name || asset.file[0].name),
								});
							}

							page.messages.add({
								type: 'red',
								countdown: 3000,
								body: page.constants.message('volume.edit.materials.remove.error', asset.name || asset.file[0].name),
							});

							asset.saving = false;
						}));
				});

				page.$q.all(promises).finally(function () {
					page.models.Slot.$cache.removeAll();
					page.models.Slot.get({
						id: form.volume.top.id,
						segment: ',',
						assets: ''
					}, function (res) {
						form.repeater.repeats.splice(0, form.repeater.repeats.length);

						angular.forEach(res.assets, function (asset) {
							asset.name = asset.asset.name;
							asset.classification = asset.asset.classification;

							form.repeater.repeats.push(asset);
						});

						angular.extend(form.slot, res);

						form.$setPristine();
					}, function (res) {
						page.messages.add({
							type: 'red',
							body: page.constants.message('volume.edit.materials.refresh.error'),
						});
					});
				});
			};

			//

			var changeFn = function () {
				form.$setDirty();
			};

			form.retrieveRepeater = function (repeater) {
				form.repeater = repeater;
				form.repeater.autoFile = form.autoFile;
				form.repeater.repeats = form.data.assets;
				form.repeater.addFn = changeFn;
				form.repeater.removeFn = changeFn;
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
