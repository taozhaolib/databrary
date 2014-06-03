module.directive('volumeEditMaterialsForm', [
	'pageService', function (page) {


		
		var post = function ($scope) {


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
						fd.append('name', asset.name || '');
						fd.append('classification', asset.classification || 0);
						fd.append('container', form.slot.container.id);
						var msg = form.messages.add({
							type: 'yellow',
							body: page.constants.message('volume.edit.materials.create', asset.name || asset.file[0].name),
						});

						promises.push(page.$http
							.post('/api/asset?volume=' + form.volume.id, fd, {
								transformRequest: angular.identity,
								headers: {
									'Content-Type': undefined
								},
							}).success(function () {
								form.messages.update(msg, {
									type: 'green',
									body: page.constants.message('volume.edit.materials.create.success', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}).error(function (res, status) {
								form.messages.addError({
									type: 'red',
									body: page.constants.message('volume.edit.materials.create.error', asset.name || asset.file[0].name),
									errors: res,
									status: status
								});
								form.messages.remove(msg);

								asset.saving = false;
							}));
					} else if (asset.asset) {
						ids.push(asset.asset.id);

						promises.push(page.$http
							.post('/api/asset/' + asset.asset.id, {
								name: asset.name || '',
								classification: asset.classification,
							}).success(function () {
								form.messages.add({
									type: 'green',
									countdown: 3000,
									body: page.constants.message('volume.edit.materials.update.success', asset.name || asset.file[0].name),
								});

								asset.saving = false;
							}).error(function (res, status) {
								form.messages.addError({
									type: 'red',
									body: page.constants.message('volume.edit.materials.update.error', asset.name || asset.file[0].name),
									errors: res,
									status: status
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
						.$delete('/api/asset/' + asset.asset.id)
						.success(function () {
							form.messages.add({
								type: 'green',
								countdown: 3000,
								body: page.constants.message('volume.edit.materials.remove.success', asset.name || asset.file[0].name),
							});

							asset.saving = false;
						}).error(function (data, status) {
							form.messages.addError({
								type: 'red',
								body: page.constants.message('volume.edit.materials.remove.error', asset.name || asset.file[0].name),
								errors: data,
								status: status
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
						var clean = false;

						angular.forEach(res.assets, function (asset) {
							if (!clean) {
								// XXX Hack #1520, don't clean out list for the first failed upload.
								// Really we should not do so for any failed upload.
								form.repeater.repeats.splice(0, form.repeater.repeats.length);
								clean = true;
							}
							asset.name = asset.asset.name;
							asset.classification = ''+asset.asset.classification;

							form.repeater.repeats.push(asset);
						});

						angular.extend(form.slot, res);

						form.$setPristine();
					}, function (res) {
						form.messages.addError({
							type: 'red',
							body: page.constants.message('volume.edit.materials.refresh.error'),
							report: res
						});
					});
				});
			};

			//
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
			
			form.getFormatsAsUnorderedList = function(){
					var DELIMIT = "</li><li>";
					var materialsFormats = page.constants.data.format;
					var x="";
					for(var i in materialsFormats)
					{
						var cur = materialsFormats[i];
						console.log(i);
						x += cur.extension +  ": " + cur.name + DELIMIT;
					}
					x = x.substr(0,x.length - DELIMIT.length);
					console.log("arf");
					return "<ul><li>"+x+"</li></ul>";
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
			link: {
				post: post 
			}
		};
	}
]);
