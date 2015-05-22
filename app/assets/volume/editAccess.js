'use strict';

app.directive('volumeEditAccessForm', [
  'pageService', '$q',
  function (page, $q) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditAccessForm;

      form.global = page.constants.accessGlobal[0].slice();
      form.data = [];
      volume.access.forEach(function (access) {
        var i = page.constants.accessGlobal.parties.indexOf(access.party.id);
        if (i >= 0)
          form.global[i] = access.children;
        else
          form.data.push(access);
      });

      form.globalVal = page.constants.accessGlobal.findIndex(function (preset) {
        return preset.every(function (p, i) {
          return form.global[i] === p;
        });
      });
      if (form.globalVal === -1)
        form.globalVal = undefined;

      var globalForm = $scope.accessGlobalForm;
      var subforms = [];

      $scope.permissionName = function (p) {
        return page.constants.permission[p];
      };

      function saveGlobal() {
        form.global = page.constants.accessGlobal[form.globalVal || 0].slice();
        $q.all(_.map(page.constants.accessGlobal.parties, function (party, i) {
          var p = form.global[i];
          volume.accessSave(party, {
            individual: p,
            children: p,
          });
        })).then(function () {
          page.messages.add({
            body: page.constants.message('access.global.save.success'),
            type: 'green',
            owner: form
          });
          form.$setPristine();
        }, function (res) {
          page.messages.addError({
            body: page.constants.message('access.global.save.error'),
            report: res,
            owner: form
          });
        });
      }

      form.saveAll = function () {
        page.messages.clear(form);
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
        if (globalForm.$dirty)
          saveGlobal();
      };

      $scope.$on('accessGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (access) {
          form.data.remove(access);
          subforms.remove(grantForm);
        };
      });

      $scope.selectFn = function (found) {
        form.data.push({
          new: true,
          party: found,
        });
        //warning: next line is template dependent! if classnames change this will no longer work
        page.display.scrollTo('fieldset .access-grant:last');
      };
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editAccess.html',
      link: link
    };
  }
]);
