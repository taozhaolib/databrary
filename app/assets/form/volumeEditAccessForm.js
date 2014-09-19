'use strict';

module.directive('volumeEditAccessForm', [
  'pageService', function (page) {
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

      form.saveAll = function () {
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
	if (globalForm.$dirty)
	  form.saveGlobal();
      };

      form.saveGlobal = function () {
	form.global = page.constants.accessGlobal[form.globalVal || 0].slice();
        page.$q.all(page.constants.accessGlobal.parties.map(function (party, i) {
          var p = form.global[i];
	  form.volume.accessSave(party, {
            individual: p,
            children: p,
          });
        })).then(function () {
          form.messages.add({
            body: page.constants.message('access.global.save.success'),
            type: 'green',
            countdown: 3000,
          });
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('access.global.save.error'),
            report: res,
          });
        });
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
	  individual: 0,
	  children: 0,
	});
	//warning: next line is template dependent! if classnames change this will no longer work
	page.display.scrollTo('fieldset .access-grant:last');
      };

      form.scrollFn = page.display.makeFloatScrollFn($('.vea-float'), $('.vea-float-floater'), 24*2.5);
      page.$w.scroll(form.scrollFn);
    };

    return {
      restrict: 'E',
      templateUrl: 'volumeEditAccessForm.html',
      link: link
    };
  }
]);
