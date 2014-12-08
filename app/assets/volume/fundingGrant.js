'use strict';

app.directive('fundingGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var funding = $scope.funding;
      var form = $scope.fundingGrantForm;

      form.data = {
        awards: funding.awards.slice(0)
      };
      form.data.awards.push('');

      if (funding.new)
        form.$setDirty();

      function keep(x) {
        return x;
      }

      form.save = function () {
        form.data.awards = form.data.awards
          .filter(keep);

        volume.fundingSave(funding.funder.id, form.data).then(function () {
          form.messages.add({
            body: page.constants.message('funding.save.success'),
            type: 'green',
            countdown: 3000,
          });

          delete funding.new;
          form.data.awards.push('');
          form.$setPristine();
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('funding.save.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.remove = function () {
        volume.fundingDelete(funding.funder.id).then(function () {
          form.messages.add({
            body: page.constants.message('funding.remove.success'),
            type: 'green',
            countdown: 3000,
          });

          form.$setPristine();
          form.removeSuccessFn(funding);
        }, function (res) {
          form.messages.addError({
            body: page.constants.message('funding.remove.error'),
            report: res,
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.awardChange = function () {
        if (form.data.awards[form.data.awards.length-1] !== '')
          form.data.awards.push('');
      };

      form.awardRemove = function (i) {
        form.data.awards[i] = i === form.data.awards.length-1 ? '' : null;
        if (i === 0 && !form.data.awards.some(keep))
          return form.remove();
        form.awardChange();
        form.$setDirty();
      };

      $scope.$emit('fundingGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/fundingGrant.html',
      link: link
    };
  }
]);
