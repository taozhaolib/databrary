'use strict';

module.directive('partyEditApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.partyEditApplyForm;

      form.data = {};

      //

      form.init = function (party, parents) {
        form.party = form.party || party;
        form.data = parents;
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

      form.scrollToFuture = function (party) {
        var remove = $scope.$watch(function () {
          return subforms[subforms.length - 1];
        }, function (subform) {
          if (subform && subform.other && subform.other.party == party) {
            page.display.scrollTo(subform.$element);
            remove();
          }
        });
      };

      //

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };

      page.events.listen($scope, 'authApplyForm-init', function (event, applyForm) {
        subforms.push(applyForm);

        applyForm.successFn = function () {
          form.messages.add({
            body: page.constants.message('auth.apply.save.success'),
            type: 'green',
            countdown: 3000,
          });
        };

        applyForm.cancelFn = function () {
          form.messages.add({
            body: page.constants.message('auth.apply.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.data.splice(form.data.indexOf(applyForm.other), 1);
          subforms.splice(subforms.indexOf(applyForm), 1);
        };

        event.stopPropagation();
      });

      page.events.listen($scope, 'authSearchForm-init', function (event, searchForm) {
        if (searchForm.principal == 'child') {
          return;
        }

        searchForm.selectFn = function (found) {
	  form.data.push({
	    new: true,
	    party: found,
	  });
        };

        searchForm.notFoundFn = function (query) {
	  form.data.push({
	    new: true,
	    query: query
	  });
        };

        event.stopPropagation();
      });

      //

      var $float = $('.peap-float');
      var $floater = $('.peap-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*1.5);
      page.$w.scroll($scope.scrollFn);


      page.events.talk('partyEditApplyForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'partyEditApplyForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
