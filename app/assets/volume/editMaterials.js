'use strict';

module.directive('volumeEditMaterialsForm', [
  'pageService', 'Store',
  function (page, Store) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var slot = volume.top;
      var form = $scope.volumeEditMaterialsForm;

      function Material(asset) {
        Store.call(this, slot, asset);
      }
      Material.prototype = Object.create(Store.prototype);
      Material.prototype.constructor = Material;

      form.materials = slot.assets.map(function (asset) {
        asset.asset.get(['creation']);
        return new Material(asset);
      });

      form.fileAdded = function (file, event) {
        var material = angular.element(event.target).scope().material;

        if (material)
          delete material.replace;
        else
          material = new Material();

        material.upload(file).then(function (done) {
          if (!done)
            return;
          if (!material.asset) {
            form.materials.push(material);
            page.display.scrollTo('fieldset.vem-repeat:last');
          }
        });
      };

      form.fileSuccess = Store.fileSuccess;
      form.fileProgress = Store.fileProgress;

      form.excerptOptions = function (material) {
        var l = {};
        for (var i = page.constants.classification.length-1; i > material.data.classification; i --)
          l[i] = page.constants.classification[i];
        l[0] = page.constants.classification[0];
        return l;
      };

      form.save = function (material) {
        if (!material.data.excerptOn)
          material.data.excerpt = '';
        material.save().then(function (done) {
          if (done)
            material.form.$setPristine();
        });
      };

      form.saveAll = function () {
        form.materials.forEach(function (material) {
          if (material.form.$dirty)
            form.save(material);
        });
      };

      form.replace = function (material) {
        material.replace = true;
        material.form.$setDirty();
      };

      function removed(material) {
        if (material.asset || material.file)
          return;
        form.materials.remove(material);
      }

      form.remove = function (material) {
        if (material.replace) {
          delete material.replace;
          return;
        }

        var r = material.remove();
        if (!(r && r.then))
          removed(material);
        else
          r.then(function (done) {
            if (done)
              removed(material);
          });
      };

      form.scrollFn = page.display.makeFloatScrollFn($('.vem-float'), $('.vem-float-floater'), 24*1.5);
      page.$w.scroll(form.scrollFn);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/editMaterials.html',
      replace: true,
      link: link
    };
  }
]);
