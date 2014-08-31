'use strict';

module.directive('volumeEditMaterialsForm', [
  'pageService', 'assetService', function (page, assets) {
    var link = function ($scope) {
      var form = $scope.volumeEditMaterialsForm;
      $scope.form = form;

      form.data = {};
      form.volume = undefined;
      form.slot = undefined;
      form.filtered = [];

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

      form.addedCall = function (file, event) {
        if (!$scope.$flow.isUploading()) {
	  //clear completed uploads so progress bar isn't pre-weighted towards completion
          while ($scope.$flow.files[0] && $scope.$flow.files[0] != file) {
            $scope.$flow.removeFile($scope.$flow.files[0]);
          }
        }

        if (angular.element(event.srcElement).scope().form) {
          //replace file
          file.asset = angular.element(event.srcElement).scope().form.subform.asset;
	  file.replace = file.asset.asset.id;
	  file.containingForm = angular.element(event.srcElement).scope().form;
        }
        else {
          //new asset
	  var index = form.add() - 1;
          file.asset = form.data.assets[index];
	  file.containingForm = form['asset' + index];
        }
        file.asset.file = file.file; 
        assets.fileAddedImmediateUpload(file);
      };

      form.assetCall = function (file) {
        var data = {};
        data.name = file.asset.name;
        data.classification = page.classification[file.asset.classification];
        data.excerpt = page.classification[file.asset.excerpt];
        data.container = form.slot.container.id;
        data.upload = file.uniqueIdentifier;

	(file.replace ?
	 file.asset.replace(data) :
	 form.volume.createAsset(data))
	  .then(function (asset) {
            file.asset = asset;
            file.asset.asset.creation = {date: Date.now(), name: file.file.name};
	    //console.log(file.containingForm);
	    if(file.containingForm && file.containingForm.subform){
	      //console.log(file.containingForm);
	      form.clean(file.containingForm.subform);
	    }
        });
      };

      form.perFileProgress = function (file) {
        file.asset.fileUploadProgress = file.progress();
      };

      form.updateExcerptChoice = function (sub, first) {
        if (first && sub.asset.asset) {
          sub.asset.excerpt = page.constants.data.classification[sub.asset.excerpt];
          return;
        }
        else if (sub.excerptOn) {
          sub.asset.excerpt = page.constants.data.classification[0];
        }
        else {
          sub.asset.excerpt = "";
        }
      };

      form.excerptOptions = function (cName) {
        var f = function (x) {
          return page.classification[x] > page.classification[cName];
        }; //string compare. if we get more than 10 must use parseInt
        var l = page.$filter('filter')(page.constants.data.classification, f);
        l.unshift(page.constants.data.classification[0]);
        return l;
      };

      $scope.totalProgress = function () {
        form.totalProgress = $scope.$flow.progress();
      };

      //

      form.save = function (subform) {
        if (angular.isFunction(form.saveFn)) {
          form.saveFn(form, subform);
        }

	var data = {};
        data.classification = page.classification[subform.asset.classification];
        if (subform.asset.excerpt === 0 || subform.asset.excerpt) {
          data.excerpt = page.classification[subform.asset.excerpt];
        }
        else {
          data.excerpt = "";
        }
        data.name = subform.asset.name || '';
        data.container = form.slot.container.id;

	if(subform.asset.asset && subform.asset.asset.creation) // NOT for file operations. just metadata
	{
	  subform.asset.asset.save(data).then(function (res) {
            subform.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('volume.edit.materials.update.success', subform.asset.name || page.constants.message('file')),
            });

            if (angular.isFunction(form.successFn)) {
              form.successFn(form, res);
            }
          }, function (res) {
            subform.messages.addError({
              type: 'red',
              body: page.constants.message('volume.edit.materials.update.error', subform.asset.name || page.constants.message('file')),
              report: res,
            });

            if (angular.isFunction(form.errorFn)) {
              form.errorFn(form, res);
            }

            page.display.scrollTo(subform.$element);
          }).finally(function () {
            form.clean(subform);
          });
        }
      };

      form.disableSaveButton = function () {
	if (!form.$dirty) return true; //for efficiency, prevent iteration if unnecessary
	var ans = true;
	angular.forEach(form, function (subform, id) {
          if (id.startsWith('asset-') && form[id] && form[id].$dirty && form[id].subform.asset.asset.creation) {
		ans = false; 
          }
        });
        return ans;
      };

      form.saveAll = function () {
        angular.forEach(form, function (subform, id) {
          if (id.startsWith('asset-') && form[id] && form[id].$dirty && form[id].subform.asset.asset.creation) { 
            form.save(subform.subform);
          }
        });
      };

      form.replace = function (subform) {
        delete subform.asset.asset.creation;
        subform.asset.file = undefined;
        subform.asset.fileUploadProgress = undefined;
      };

      form.remove = function (subform) {
        if (angular.isFunction(form.removeFn)) {
          form.removeFn(form, subform);
        }

        if (!subform.asset.asset) {
          form.clean(subform);
          form.data.assets.splice(form.data.assets.indexOf(subform.asset), 1);
        } else {
	  subform.asset.remove().then(function (res) {
            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('volume.edit.materials.remove.success', subform.asset.name || page.constants.message('file')),
            });

            if (angular.isFunction(form.successFn)) {
              form.successFn(form, res);
            }

            form.data.assets.splice(form.data.assets.indexOf(subform.asset), 1);

          }, function (res) {
            form.messages.addError({
              type: 'red',
              body: page.constants.message('volume.edit.materials.remove.error', subform.asset.name || page.constants.message('file')),
              report: res,
            });

            if (angular.isFunction(form.errorFn)) {
              form.errorFn(form, res);
            }

            page.display.scrollTo(subform.$element);
          }).finally(function () {
            form.clean(subform);
          });
        }
      };

      form.add = function () {
        if (angular.isFunction(form.addFn)) {
          form.saveFn(form);
        }

        return form.data.assets.push({
          classification: 'SHARED',
          excerpt: ''
        });
      };

      form.clean = function (subform) {
        if (subform) {
          subform.form.$setPristine();
        }

        var pristine = true;

        angular.forEach(form, function (subform, id) {
          if (id.startsWith('asset-') && form[id] && form[id].$dirty) {
            pristine = false;
            return false;
          }
        });

        if (pristine) {
          form.$setPristine();
        }
      };


      var scrollPosX = window.pageXOffset;
      var scrollPosY = window.pageYOffset;
      form.resetAll = function (force) {
	  if(force || confirm(page.constants.message('navigation.confirmation'))){
	    scrollPosX = window.pageXOffset;
	    scrollPosY = window.pageYOffset;
	    page.$route.reload();
	    page.$timeout(function() {window.scrollTo(scrollPosX,scrollPosY);});
	    return true;
	  }
	  return false;
      };

      form.makeThumbData = function (context) {
        return{
          container: context.volumeEditMaterialsForm.slot.container,
          asset: context.asset.asset
        };
      };

      var $float = $('.vem-float');
      var $floater = $('.vem-float-floater');

      page.$w.scroll(function () {
        if (window.pageYOffset + (24 * 1.5) >= $float.offset().top) {
          $floater.addClass('float');
        } else {
          $floater.removeClass('float');
        }
      });

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
