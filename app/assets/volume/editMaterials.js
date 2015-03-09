'use strict';

app.directive('volumeEditMaterialsForm', [
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

      Material.prototype.fillData = function () {
        Store.prototype.fillData.call(this);
        if (this.asset) {
          this.data.excerptOn = this.asset.excerpt != null;
          this.data.excerpt = (this.asset.excerpt || 0)+'';
        }
      };

      Material.prototype.save = function () {
        if (!this.data.excerptOn)
          this.data.excerpt = '';
        else if (this.data.excerpt <= this.data.classification)
          this.data.excerpt = '0';
        return Store.prototype.save.call(this);
      };

      form.materials = [];
      angular.forEach(slot.assets, function (asset) {
        asset.get(['creation']);
        form.materials.push(new Material(asset));
      });

      form.fileAdded = function (file, event) {
        var target = event.target;
        if (target.tagName === 'INPUT')
          target = target.parentElement;
        var material;
        if (target.id.startsWith('material-drop-')) {
          material = form.materials[target.id.substr(14)];
          delete material.replace;
        } else
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

      form.save = function (material) {
        if (material.form.$pristine)
          return;
        if (material.file) /* upload in progress */
          return material.form.$setPristine();
        material.save().then(function (done) {
          if (done)
            material.form.$setPristine();
        });
      };

      form.saveAll = function () {
        form.materials.forEach(form.save);
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
