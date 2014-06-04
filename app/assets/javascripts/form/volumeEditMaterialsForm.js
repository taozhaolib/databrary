module.directive('volumeEditMaterialsForm', [
	'pageService', function (page) {


		
		var post = function ($scope) {


			var form = $scope.volumeEditMaterialsForm;
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
				form.volume = form.volume || volume;
			};

			//

			form.save = function (subform) {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form, subform);
				}

				if (subform.asset.file) {
						var fd = new FormData();
					fd.append('file', subform.asset.file[0]);
					fd.append('name', subform.asset.name || '');
					fd.append('classification', subform.asset.classification || 0);
						fd.append('container', form.slot.container.id);

					var msg = subform.messages.add({
							type: 'yellow',
						body: page.constants.message('volume.edit.materials.create', subform.asset.name || subform.asset.file[0].name),
						});

					page.models.Asset.upload(form.volume, fd)
						.then(function (res) {
							subform.messages.update(msg, {
									type: 'green',
								body: page.constants.message('volume.edit.materials.create.success', subform.asset.name || subform.asset.file[0].name),
								});

							subform.$setPristine();
						}, function (res) {
							subform.messages.addError({
									type: 'red',
								body: page.constants.message('volume.edit.materials.create.error', subform.asset.name || subform.asset.file[0].name),
								report: res,
								});

							subform.messages.remove(msg);

							subform.form.$setPristine();
						});
				} else {
					var newAsset = new page.models.Asset({
						name: subform.asset.name || '',
						classification: subform.asset.classification,
					});

					newAsset.$save({
						id: subform.asset.asset.id
					}).then(function (res) {
						subform.messages.add({
									type: 'green',
									countdown: 3000,
							body: page.constants.message('volume.edit.materials.update.success', subform.asset.name || subform.asset.file[0].name),
								});

						form.clean(subform);
					}, function (res) {
						subform.messages.addError({
									type: 'red',
							body: page.constants.message('volume.edit.materials.update.error', subform.asset.name || subform.asset.file[0].name),
							report: res,
								});

						form.clean(subform);
					});
					}
			};

			form.remove = function (subform) {
				if (angular.isFunction(form.removeFn)) {
					form.saveFn(form, subform);
					}

				if (!subform.asset.asset) {
					form.data.assets.splice(subform.$index, 1);
				} else {
					var newAsset = new page.models.Asset();

					newAsset.$delete({
						id: subform.asset.asset.id
					}).then(function (res) {
							form.messages.add({
								type: 'green',
								countdown: 3000,
							body: page.constants.message('volume.edit.materials.remove.success', subform.asset.name || subform.asset.file[0].name),
							});

						form.data.assets.splice(subform.$index, 1);

						form.clean(subform);
					}, function (res) {
							form.messages.addError({
								type: 'red',
							body: page.constants.message('volume.edit.materials.remove.error', subform.asset.name || subform.asset.file[0].name),
								errors: data,
								status: status
							});

						form.clean(subform);
				});
				}
			};

			form.add = function () {
				if (angular.isFunction(form.addFn)) {
					form.saveFn(form);
							}

				return form.data.assets.push({
					classification: '1',
						});
			};

			form.clean = function (subform) {
				subform.form.$setPristine();

				var pristine = true;
				
				angular.forEach(form, function (subform, id) {
					if (id.indexOf('asset-') === 0 && form[id].$dirty) {
						pristine = false;
						return false;
					}
				});
				
				if (pristine) {
						form.$setPristine();
				}
			};

			form.store = function (subform) {
				backup[subform.$index] = $.extend(true, {}, subform.asset);
			};

			form.reset = function (subform) {
				subform.asset = backup[subform.$index];
				form.store(subform);
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
