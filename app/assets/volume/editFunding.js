'use strict';

app.directive('volumeEditFundingForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditFundingForm;

      form.data = volume.funding.slice();

      var subforms = [];

      form.saveAll = function () {
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
      };

      $scope.$on('fundingGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (funding) {
          form.data.remove(funding);
          subforms.remove(grantForm);
        };
      });

      $scope.selectFn = function (found) {
        if (form.data.some(function (funding) {
              return funding.funder.id === found.id;
            })) {
          form.messages.add({
            type: 'yellow',
            countdown: 3000,
            body: page.constants.message('funding.search.repeat', found.name),
          });
          return;
        }

        form.data.push({
          funder: found,
          awards: [],
          new: true,
        });

        //warning: next line is template dependent! if classnames or structure change this may no longer work
        page.display.scrollTo('fieldset.funding-grant:last');
      };
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/editFunding.html',
      link: link
    };
  }
]);
