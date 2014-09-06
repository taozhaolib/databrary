'use strict';

module.directive('volumeEditFundingForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.volumeEditFundingForm;

      form.data = {};
      form.volume = undefined;

      //

      form.init = function (data, volume) {
        form.data = data;
        form.volume = form.volume || volume;
      };

      //

      var subforms = [];

      $scope.$watch(function () {
        var clean = true;

        angular.forEach(subforms, function (subform) {
          if (subform.$dirty) {
            clean = false;
            return false;
          }
        });

        if (clean) {
          form.$setPristine();
        }
      });

      form.saveAll = function () {
        angular.forEach(subforms, function (subform) {
          if (subform.$dirty) {
            subform.save(false);
          }
        });
      };

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };

      //

      var $float = $('.vef-float');
      var $floater = $('.vef-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*2.5);
      page.$w.scroll($scope.scrollFn);

      $scope.$on('fundingGrantForm-init', function (event, grantForm) {
        grantForm.volume = form.volume;
        if (grantForm.data.new) {
          grantForm.$setDirty();
        }
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function () {
          form.data.splice(form.data.indexOf(this.data), 1);
        };

        event.stopPropagation();
      });

      $scope.$on('fundingSearchForm-init', function (event, searchForm) {
        searchForm.volume = form.volume;

        searchForm.selectFn = function (found) {
          var present = false;

          angular.forEach(form.data, function (funder, i) {
            if (funder.funder.id === found.id) {
              var el = form.data.splice(i, 1)[0];
              form.data.push(el);
              present = true;
              return false;
            }
          });

          if (!present) {
            form.data.push({
              funder: found,
              awards: [],
              new: true,
            });
          } else {
            searchForm.messages.add({
              type: 'yellow',
              countdown: 3000,
              body: page.constants.message('funding.search.repeat', found.name),
            });
          }
        };

        event.stopPropagation();
      });
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volumeEditFundingForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
