'use strict';

module.directive('volumeEditAccessForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.volumeEditAccessForm;

      form.data = [];
      form.global = angular.copy(page.constants.accessGlobal[0]);

      form.volume = undefined;

      //

      form.init = function (data, volume) {
        if (form.data.length === 0) {
          angular.forEach(data, function (access) {
            var i = page.constants.accessGlobal.parties.indexOf(access.party.id);
            if (i >= 0) {
              form.global[i] = page.constants.permission[access.children || 0];
            } else {
              form.data.push(access);
            }
          });
          form.calcGlobalVal();

          form.volume = form.volume || volume;
        }
      };

      form.calcGlobalVal = function () {
        form.globalVal = undefined;

        angular.forEach(page.constants.accessGlobal, function (preset, i) {
          if (preset.every(function (x, i) {
            return form.global[i] === x;
          })) {
            form.globalVal = i;
          }
        });
      };

      form.changeAccessGlobal = function () {
        angular.copy(page.constants.accessGlobal[form.globalVal], form.global);
        form.accessGlobalDirty = true;
        form.$setDirty();
      };

      //

      var subforms = [];

      $scope.$watch(function () {
	if (subforms.every(function(x){return !x.$dirty;}) && !form.accessGlobalDirty){
          form.$setPristine();
        }
      });

      form.saveAll = function () {
        angular.forEach(subforms, function (subform) {
          if (subform.$dirty) {
            subform.save(false);
          }
        });
	if (form.accessGlobalDirty){
	  form.saveGlobal();
	}
      };

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };

      //

      form.saveGlobal = function () {
        page.$q.all(page.constants.accessGlobal.parties.map(function (party, i) {
          var p = page.constants.permissionName[form.global[i]];
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

          form.accessGlobalDirty = false;
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('access.global.save.error'),
            report: res,
          });
          page.display.scrollTo(form.$element);
        });
      };

      //

      page.events.talk('volumeEditAccessForm-init', form, $scope);

      //

      $scope.$on('accessGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (access) {
          form.data.splice(form.data.indexOf(access), 1);
        };

        event.stopPropagation();
      });

      var $float = $('.vea-float');
      var $floater = $('.vea-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll($scope.scrollFn);
      
      $scope.$on('accessSearchForm-init', function (event, searchForm) {
        searchForm.selectFn = function (found) {
	  form.data.push({
	    new: true,
	    party: found,
	    individual: 0,
	    children: 0,
	  });
        };

        event.stopPropagation();
      });
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volumeEditAccessForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
